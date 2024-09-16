use anyhow::{Context, Result};
use clap::Parser;
use itertools::Itertools;
use serde::Serialize;

use pyo3::types::{IntoPyDict, PyList};
use pyo3::{prelude::*, types::PyTuple};

use expr::PostExpectation;
use path::Path;
use std::{
    fmt::Display,
    fs::{File, OpenOptions},
    io::Write,
};
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
    /// print the pre-expectation in the Python language
    python: bool,

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
    /// The computed pre-expectation expressed as a Python symbolic expression.
    ///
    /// This field is `Some(_)` iff the user provided both the `--python` and `--post-expectation` flags.
    #[serde(skip_serializing_if = "Option::is_none")]
    python_pre_expectation: Option<String>,
}

impl Report {
    fn new(
        paths: Vec<Path>,
        num_removed_paths: usize,
        num_uniform_samples: u32,
        num_normal_samples: u32,
    ) -> Self {
        let num_paths = paths.len();

        let mut sample_variables: Vec<String> = Vec::new();
        let mut int_ranges: Vec<String> = Vec::new();
        for k in 0..num_uniform_samples {
            sample_variables.push(format!("y_{}", k));
            int_ranges.push("[0,1]".to_string());
        }
        for k in 0..num_normal_samples {
            sample_variables.push(format!("z_{}", k));
            int_ranges.push("[-20,20]".to_string());
        }
        let vars = sample_variables.iter().join(",");
        let int_range = int_ranges.iter().join(",");
        let python_pre_expectation = Some(
            format!("\nfrom scipy import integrate\nfrom math import *\ndef f (*args):\n  {} = args\n  return \\ \n  ",vars).to_owned()+
            &paths
                .iter()
                .flat_map(|p| p.python_preexpectation())
                .join("\\ \n  +")+
            &format!("\nr,e = integrate.nquad(f,[{}])",int_range)+
            &format!("\nprint(\"result = \",r)\nprint(\"error  = \",e)"),
        )
        .filter(|preexp| !preexp.is_empty());

        Self {
            num_paths,
            num_removed_paths,
            num_uniform_samples,
            num_normal_samples,
            paths,
            python_pre_expectation,
        }
    }
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
        if let Some(preexp) = &self.python_pre_expectation {
            writeln!(f, "[Python] Pre-expectation: {preexp}")
        } else {
            Ok(())
        }
    }
}

fn python_version() -> PyResult<()> {
    Python::with_gil(|py| {
        let sys = py.import_bound("sys")?;
        let version: String = sys.getattr("version")?.extract()?;

        let locals = [("os", py.import_bound("os")?)].into_py_dict_bound(py);
        let code = "os.getenv('USER') or os.getenv('USERNAME') or 'Unknown'";
        let user: String = py.eval_bound(code, None, Some(&locals))?.extract()?;

        println!("Hello {}, I'm Python {}", user, version);
        Ok(())
    })
}

fn main() -> Result<(), anyhow::Error> {
    python_version()?;
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
    let (mut paths, num_failed_observe_paths) = executor.run(args.prob)?;

    // If a postcondition was provided, add it to all the paths.
    //
    // If an error is encountered while typechecking the postcondition against each path's
    // substitution, return that error immediately.
    if let Some(post) = postexpectation {
        paths
            .iter_mut()
            .zip(std::iter::repeat(post))
            .try_for_each(|(path, post)| path.add_postexpectation(post))?;

        // Enable the creation of pre-expectation expressions represented as an integral in the
        // Python language if the user asked for one.
        if args.python {
            for p in paths.iter_mut() {
                p.set_python_output();
            }
        }
    }

    // Compute the maximum number of uniform samples across all the paths.
    let num_uniform_samples = paths.iter().map(|p| p.num_uniform_samples).max().unwrap();

    // Compute the maximum number of uniform samples across all the paths.
    let num_normal_samples = paths.iter().map(|p| p.num_normal_samples).max().unwrap();

    // Construct the report to give to the user.
    let report = Report::new(
        paths,
        num_failed_observe_paths,
        num_uniform_samples,
        num_normal_samples,
    );

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
