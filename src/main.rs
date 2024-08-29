use anyhow::{Context, Result};
use clap::Parser;
use itertools::Itertools;
use serde::Serialize;

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
mod semantics;
mod smt;
mod syntax;
mod wolfram;

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

    #[arg(short, long)]
    /// print the pre-expectation in the Wolfram language
    wolfram: bool,

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
    /// The total number of unique probabilistic samples encountered.
    num_samples: u32,
    /// The set of explored paths.
    paths: Vec<Path>,
    /// The computed pre-expectation expressed as a Wolfram symbolic expression.
    ///
    /// This field is `Some(_)` iff the user provided both the `--wolfram` and `--post-expectation` flags.
    #[serde(skip_serializing_if = "Option::is_none")]
    wolfram_pre_expectation: Option<String>,
}

impl Report {
    fn new(paths: Vec<Path>, num_removed_paths: usize, num_samples: u32) -> Self {
        let num_paths = paths.len();

        let wolfram_pre_expectation = Some(
            paths
                .iter()
                .flat_map(|p| p.wolfram_preexpectation())
                .join("+"),
        )
        .filter(|preexp| !preexp.is_empty());

        Self {
            num_paths,
            num_removed_paths,
            num_samples,
            paths,
            wolfram_pre_expectation,
        }
    }
}

impl Display for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Number of Paths: {}", self.num_paths)?;
        writeln!(f, "Number of Removed Paths: {}", self.num_removed_paths)?;
        writeln!(f, "Number of Samples: {}", self.num_samples)?;
        for (i, path) in self.paths.iter().enumerate() {
            writeln!(f, "Path {}:\n{}", i + 1, path)?;
        }
        if let Some(preexp) = &self.wolfram_pre_expectation {
            writeln!(f, "[Wolfram] Pre-expectation: {preexp}")
        } else {
            Ok(())
        }
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
        // Wolfram language if the user asked for one.
        if args.wolfram {
            for p in paths.iter_mut() {
                p.set_wolfram_output();
            }
        }
    }

    // Compute the maximum number of random samples across all the paths. That is, find the path
    // which had the most random samples and return the number of samples that path made.
    let num_samples = paths
        .iter()
        .map(|p| p.num_uniform_samples + p.num_normal_samples)
        .max()
        .unwrap();

    // Construct the report to give to the user.
    let report = Report::new(paths, num_failed_observe_paths, num_samples);

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
