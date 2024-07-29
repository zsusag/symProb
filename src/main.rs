use anyhow::Result;
use clap::Parser;

use csv::WriterBuilder;
use expr::PostExpectation;
use path::Path;
use serde::Serialize;
use std::{
    fs::File,
    io::{self, Write},
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

#[derive(Serialize)]
pub struct CSVRow {
    path_num: usize,
    terminated: bool,
    num_uniform_samples: u32,
    num_normal_samples: u32,
    pc: String,
    po: String,
    pr_pc: Option<String>,
    pr_po: Option<String>,
    sigma: Vec<Option<String>>,
    preexp: Option<String>,
}

pub fn gen_csv_header<'a, I>(paths: I) -> (Vec<String>, BTreeSet<&'a String>)
where
    I: IntoIterator<Item = &'a Path>,
{
    let all_var_names: BTreeSet<_> = paths
        .into_iter()
        .flat_map(|p| p.get_sigma().variables())
        .collect();

    let mut csv_header = vec![
        "Path".to_string(),
        "Forced termination".to_string(),
        "# Uniform Samples".to_string(),
        "# Normal Samples".to_string(),
        "PC".to_string(),
        "PO".to_string(),
        "Pr(PC)".to_string(),
        "Pr(PO)".to_string(),
    ];

    csv_header.extend(
        all_var_names
            .clone()
            .into_iter()
            .map(|var_name| format!("σ({var_name})")),
    );

    (csv_header, all_var_names)
}

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
    /// output in CSV format
    csv: bool,

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

    if args.csv {
        let (header, all_var_names) = gen_csv_header(&paths);
        let rows = paths
            .iter()
            .enumerate()
            .map(|(i, p)| p.to_csv_row(i, &all_var_names));
        match args.output {
            Some(path) => {
                let mut wtr = WriterBuilder::new().has_headers(false).from_path(path)?;
                wtr.write_record(header)?;
                for row in rows {
                    wtr.serialize(row)?;
                }
                wtr.flush()?;
            }
            None => {
                let mut wtr = WriterBuilder::new()
                    .has_headers(false)
                    .from_writer(io::stdout());
                wtr.write_record(header)?;
                for row in rows {
                    wtr.serialize(row)?;
                }
                wtr.flush()?;
            }
        }
    } else if let Some(output_path) = args.output {
        let mut f = File::create(output_path)?;
        writeln!(f, "Number of Paths: {}", paths.len())?;
        writeln!(f, "Number of Removed Paths: {num_failed_observe_paths}")?;
        writeln!(f, "Number of Samples: {num_samples}")?;
        for (i, path) in paths.iter().enumerate() {
            writeln!(f, "Path {}:\n{}", i + 1, path)?;
        }

        f.flush()?;
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
