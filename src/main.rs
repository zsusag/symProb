use anyhow::{Context, Result};
use clap::Parser;
use executor::ExecutorReport;
use itertools::Itertools;
use python::PyPathPreExpectation;
use serde::Serialize;

use pyo3::prelude::*;
use pyo3::types::IntoPyDict;

use expr::PostExpectation;
use path::Path;
use std::{
    fmt::Display,
    fs::{File, OpenOptions},
    io::Write,
};
use symbolic::{SymType, SymVarMap};
use syntax::FnMap;

use crate::{executor::Executor, parser::parse_file, semantics::check_valid_program};

mod executor;
mod executor_state;
mod expr;
mod parser;
mod path;
mod probability;
mod psi_parser;
mod python;
mod semantics;
mod smt;
mod symbolic;
mod syntax;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// path to probabilistic programs
    p: std::path::PathBuf,

    #[arg(short, long)]
    /// maximum number of times to unroll all the while loops
    max_iterations: Option<u32>,

    #[arg(short, long)]
    /// calculate the probability of the path condition and path observation via PSI
    prob: bool,

    #[arg(long)]
    /// output in JSON format
    json: bool,

    #[arg(short, long)]
    /// overwrite output file if it already exists
    force: bool,

    #[arg(short, long)]
    /// output to file
    output: Option<std::path::PathBuf>,

    #[arg(long)]
    /// A post-expectation expression parameterized by program variables.
    post_expectation: Option<String>,

    #[arg(long)]
    /// integrate the pre-expectation expression using SymPy
    integrate: bool,

    #[arg(long)]
    /// compute the normalized pre-expectation
    normalize: bool,

    #[arg(long)]
    /// Path to a post-expectation expression parameterized by program variables stored within a
    /// file.
    post_expectation_path: Option<std::path::PathBuf>,
}

/// The set of data to report to the user upon termination.
#[derive(Serialize)]
struct Report {
    /// The number of explored paths.
    num_paths: usize,
    /// The number of paths which were pruned.
    num_removed_paths: usize,
    /// The total number of uniform samples encountered.
    num_uniform_samples: u32,
    /// The total number of normal samples encountered.
    num_normal_samples: u32,
    /// The set of explored paths.
    paths: Vec<Path>,
    /// Whether the pre-expectation is an underapproximation (i.e., some paths were forcibly
    /// terminated)
    #[serde(skip_serializing_if = "Option::is_none")]
    pre_expectation_underapproximation: Option<bool>,
    /// The exact, unnormalized pre-expectation expressed as a [SymPy](https://docs.sympy.org) symbolic expression.
    /// This field is `Some(_)` if the user provided both the `--integrate` and `--post-expectation` flags.
    #[serde(skip_serializing_if = "Option::is_none")]
    unnormalized_pre_expectation_exact: Option<String>,
    /// The approximate, unnormalized pre-expectation expressed as a [SymPy](https://docs.sympy.org) symbolic expression.
    ///
    /// This field is `Some(_)` if the user provided both the `--integrate` and `--post-expectation` flags.
    #[serde(skip_serializing_if = "Option::is_none")]
    unnormalized_pre_expectation_approx: Option<f64>,

    /// The exact normalization constant expressed as a [SymPy](https://docs.sympy.org) symbolic expression.
    /// This field is `Some(_)` if the user provided both the `--integrate` and `--post-expectation` flags.
    #[serde(skip_serializing_if = "Option::is_none")]
    normalization_constant_exact: Option<String>,
    /// The approximate normalization constant expressed as a [SymPy](https://docs.sympy.org) symbolic expression.
    ///
    /// This field is `Some(_)` if the user provided both the `--integrate` and `--post-expectation` flags.
    #[serde(skip_serializing_if = "Option::is_none")]
    normalization_constant_approx: Option<f64>,

    /// The exact, normalized pre-expectation expressed as a [SymPy](https://docs.sympy.org) symbolic expression.
    /// This field is `Some(_)` if the user provided both the `--integrate` and `--post-expectation` flags.
    #[serde(skip_serializing_if = "Option::is_none")]
    pre_expectation_exact: Option<String>,
    /// The approximate, normalized pre-expectation expressed as a [SymPy](https://docs.sympy.org) symbolic expression.
    ///
    /// This field is `Some(_)` if the user provided both the `--integrate` and `--post-expectation` flags.
    #[serde(skip_serializing_if = "Option::is_none")]
    pre_expectation_approx: Option<f64>,
}

impl Report {
    fn new(
        paths: Vec<Path>,
        num_removed_paths: usize,
        sym_vars: SymVarMap,
        integrate: bool,
        normalize: bool,
    ) -> Result<Self> {
        let num_paths = paths.len();
        let dists = sym_vars.values().filter_map(|st| {
            if let SymType::Prob(dist) = st {
                Some(dist)
            } else {
                None
            }
        });
        let num_uniform_samples: u32 = dists
            .clone()
            .filter(|d| d.is_uniform())
            .count()
            .try_into()?;
        let num_normal_samples: u32 = dists.filter(|d| d.is_normal()).count().try_into()?;

        let pre_expectation = if integrate {
            paths
                .iter()
                .filter(|p| !p.forcibly_terminated())
                .map(|p| p.integrable_preexpectation())
                .collect::<Option<Vec<_>>>()
                .and_then(|path_preexps| {
                    if !path_preexps.is_empty() {
                        Some(
                            integrate_preexp(path_preexps, &sym_vars)
                                .context("SymPy could not compute the pre-expectation"),
                        )
                    } else {
                        None
                    }
                })
                .transpose()?
        } else {
            None
        };

        let pre_expectation_underapproximation = if pre_expectation.is_some() {
            Some(paths.iter().any(|p| p.forcibly_terminated()))
        } else {
            None
        };

        let (unnormalized_pre_expectation_exact, unnormalized_pre_expectation_approx) =
            match pre_expectation {
                Some((exact, approx)) => (Some(exact), Some(approx)),
                None => (None, None),
            };

        if normalize {
            let constant = if integrate {
                paths
                    .iter()
                    .filter(|p| !p.forcibly_terminated())
                    .map(|p| p.integrable_preexp_normal_const())
                    .collect::<Option<Vec<_>>>()
                    .and_then(|cnsts| {
                        if !cnsts.is_empty() {
                            Some(
                                integrate_preexp(cnsts, &sym_vars)
                                    .context("Python could not compute the pre-expectation"),
                            )
                        } else {
                            None
                        }
                    })
                    .transpose()?
            } else {
                None
            };

            let (normalization_constant_exact, normalization_constant_approx) = match constant {
                Some((exact, approx)) => (Some(exact), Some(approx)),
                None => (None, None),
            };

            let pre_expectation_exact = unnormalized_pre_expectation_exact
                .as_ref()
                .zip(normalization_constant_exact.as_ref())
                .map(|(preexp, cnst)| divide_exact(preexp, cnst))
                .transpose()
                .context("SymPy could not compute the normalized pre-expectation")?;

            let pre_expectation_approx = unnormalized_pre_expectation_approx
                .zip(normalization_constant_approx)
                .map(|(preexp, cnst)| preexp / cnst);

            Ok(Self {
                num_paths,
                num_removed_paths,
                num_uniform_samples,
                num_normal_samples,
                paths,
                pre_expectation_underapproximation,
                unnormalized_pre_expectation_exact,
                unnormalized_pre_expectation_approx,
                normalization_constant_exact,
                normalization_constant_approx,
                pre_expectation_exact,
                pre_expectation_approx,
            })
        } else {
            Ok(Self {
                num_paths,
                num_removed_paths,
                num_uniform_samples,
                num_normal_samples,
                paths,
                pre_expectation_underapproximation,
                unnormalized_pre_expectation_exact,
                unnormalized_pre_expectation_approx,
                normalization_constant_exact: None,
                normalization_constant_approx: None,
                pre_expectation_exact: None,
                pre_expectation_approx: None,
            })
        }
    }
}

fn integrate_preexp<I>(path_preexps: I, sym_vars: &SymVarMap) -> Result<(String, f64)>
where
    I: IntoIterator<Item = PyPathPreExpectation>,
{
    let preexp_integrand = path_preexps.into_iter().join("+");

    let symbols_arg = sym_vars.keys().join(",");

    let sympy_integrate_limits = sym_vars
        .keys()
        .zip(sym_vars.values().filter_map(|st| {
            if let SymType::Prob(dist) = st {
                Some(dist.range())
            } else {
                None
            }
        }))
        .map(|(var, limits)| format!("({var},{},{})", limits.start(), limits.end()))
        .join(", ");

    let sympy_integrate = format!(
        r#"
import sympy as sp
from sympy.parsing.sympy_parser import parse_expr

def integrate(preexp):
    expr = parse_expr(str(preexp))
    {SYMVARS} = sp.symbols("{SYMVARS}")
    r = sp.integrate(expr, {LIMITS})
    r_eval = r.evalf()
    return (str(r), r_eval)
"#,
        LIMITS = &sympy_integrate_limits,
        SYMVARS = &symbols_arg
    );
    Python::with_gil(|py| {
        let integrate =
            PyModule::from_code_bound(py, &sympy_integrate, "integrate.py", "integrate")?;
        let (r, r_eval): (String, f64) = integrate
            .getattr("integrate")
            .context("getattr")?
            .call1((&preexp_integrand,))
            .context("call1")?
            .extract()
            .context("extract")?;
        Ok((r, r_eval))
    })
}

fn divide_exact(numer: &str, denom: &str) -> Result<String> {
    Python::with_gil(|py| {
        let sympy_parser = PyModule::import_bound(py, "sympy.parsing.sympy_parser")?;
        let p = sympy_parser
            .getattr("parse_expr")?
            .call1((numer,))
            .context("error converting expression into a SymPy expression")?;
        let q = sympy_parser
            .getattr("parse_expr")?
            .call1((denom,))
            .context("error converting expression into a SymPy expression")?;
        let args = [("p", p), ("q", q)].into_py_dict_bound(py);
        let res: String = py
            .eval_bound("str(p/q)", None, Some(&args))
            .context("error dividing")?
            .extract()?;
        Ok(res)
    })
}

impl Display for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Number of paths: {}", self.num_paths)?;
        writeln!(f, "Number of removed paths: {}", self.num_removed_paths)?;
        writeln!(f, "Number of uniform samples: {}", self.num_uniform_samples)?;
        writeln!(f, "Number of normal samples: {}", self.num_normal_samples)?;
        for (i, path) in self.paths.iter().enumerate() {
            writeln!(f, "Path {}:\n{}", i + 1, path)?;
        }
        if let Some(underapprox) = self.pre_expectation_underapproximation {
            if underapprox {
                writeln!(
                    f,
                    "WARNING: The below pre-expectations are an underapproximation as not all paths could be explored."
                )?;
            }
        }
        if let Some(pe) = &self.unnormalized_pre_expectation_exact {
            writeln!(f, "Unnormalized Pre-expectation = {pe}")?;
            writeln!(
                f,
                "Unnormalized Pre-expectation ≈ {}",
                self.unnormalized_pre_expectation_approx.unwrap()
            )?;
        }
        if let Some(cnst) = &self.normalization_constant_exact {
            writeln!(f, "Normalization Constant = {cnst}")?;
            writeln!(
                f,
                "Normalization Constant ≈ {}",
                self.normalization_constant_approx.unwrap()
            )?;
        }
        if let Some(pe) = &self.pre_expectation_exact {
            writeln!(f, "Pre-expectation = {pe}")?;
            writeln!(
                f,
                "Pre-expectation ≈ {}",
                self.pre_expectation_approx.unwrap()
            )?;
        }
        Ok(())
    }
}

fn main() -> Result<(), anyhow::Error> {
    // Parse the command line arguments.
    let args = Args::parse();

    // Parse the provided program.
    let fn_defs: FnMap = parse_file(&args.p)?;

    // Check whether the provided program is semantically valid.
    check_valid_program(&fn_defs)?;

    // Parse the postexpectation expression, if provided.
    let postexpectation = args
        .post_expectation
        .map(|data| PostExpectation::parse(&data))
        .transpose()?;

    // Create a new symbolic executor for the provided program.
    let executor = Executor::new(fn_defs, &args.max_iterations);

    // Find all the paths (within a finite number of iterations) and optionally compute their
    // probabilities.
    let ExecutorReport {
        mut paths,
        sym_vars,
        num_failed_observe_paths,
    } = executor.run(args.prob)?;

    // If a postcondition was provided, add it to all the paths.
    //
    // If an error is encountered while typechecking the postcondition against each path's
    // substitution, return that error immediately.
    if let Some(post) = postexpectation {
        paths
            .iter_mut()
            .zip(std::iter::repeat(post))
            .try_for_each(|(path, post)| path.add_postexpectation(post))?;
    }

    // Construct the report to give to the user.
    let report = Report::new(
        paths,
        num_failed_observe_paths,
        sym_vars,
        args.integrate,
        args.normalize,
    )?;

    if args.json {
        if let Some(output_path) = args.output {
            let output = output_path.with_extension("json");
            // Create a new JSON file for output, erroring if the file already exists.
            {
                let f = if args.force {
                    OpenOptions::new()
                        .create(true)
                        .truncate(false)
                        .write(true)
                        .open(&output)
                } else {
                    File::create_new(&output)
                }
                .with_context(|| {
                    format!("failed to open JSON output file at {}", output.display())
                })?;

                serde_json::to_writer_pretty(f, &report)
                    .context("failed serializing found paths to JSON")?;
            }
            println!("Explored paths have been written to {}", output.display());
        } else {
            println!(
                "{}",
                serde_json::to_string_pretty(&report)
                    .context("failed serializing found paths to JSON")?
            );
        }
    } else if let Some(output_path) = args.output {
        // Create a new output file, erroring if the file already exists.
        let output = output_path.with_extension("txt");
        let mut f = if args.force {
            OpenOptions::new()
                .create(true)
                .truncate(false)
                .write(true)
                .open(&output)
        } else {
            File::create_new(&output)
        }
        .with_context(|| format!("failed to open output file at {}", output.display()))?;

        writeln!(f, "{report}")?;
        println!("Explored paths have been written to {}", output.display());
    } else {
        println!("{report}");
    }
    Ok(())
}
