use anyhow::{Context, Result};
use clap::Parser;

use expr::PostExpectation;
use std::{
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

    #[arg(long)]
    /// Path to a post-expectation expression parameterized by program variables stored within a
    /// file.
    post_expectation_path: Option<std::path::PathBuf>,
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
    }

    // Compute the maximum number of random samples across all the paths. That is, find the path
    // which had the most random samples and return the number of samples that path made.
    let num_samples = paths
        .iter()
        .map(|p| p.num_uniform_samples + p.num_normal_samples)
        .max()
        .unwrap();

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

                serde_json::to_writer_pretty(f, &paths)
                    .context("failed serializing found paths to JSON")?;
            }
            println!("Explored paths have been written to {}", output.display());
        } else {
            println!(
                "{}",
                serde_json::to_string_pretty(&paths)
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

        writeln!(f, "Number of Paths: {}", paths.len())?;
        writeln!(f, "Number of Removed Paths: {num_failed_observe_paths}")?;
        writeln!(f, "Number of Samples: {num_samples}")?;
        for (i, path) in paths.iter().enumerate() {
            writeln!(f, "Path {}:\n{}", i + 1, path)?;
        }
        println!("Explored paths have been written to {}", output.display());
    } else {
        println!("Number of Paths: {}", paths.len());
        println!("Number of Removed Paths: {num_failed_observe_paths}");
        println!("Number of Samples: {num_samples}");
        for (i, path) in paths.iter().enumerate() {
            println!("Path {}:\n{}", i + 1, path);
        }
    }
    Ok(())
}
