use anyhow::{Context, Result};
use clap::Parser;

use csv::WriterBuilder;
use path::gen_csv_header;
use pest::Parser as EParser;
use std::{
    fs::{self, File},
    io::{self, Write},
};

use crate::{
    executor::Executor,
    parser::{parse_file, ExprParser},
    semantics::check_valid_program,
};

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
}

fn main() -> Result<(), anyhow::Error> {
    let args = Args::parse();

    // Need to check whether file exists
    let f = fs::read_to_string(args.p.as_path())
        .with_context(|| format!("Failed to read program from {}", args.p.as_path().display()))?;

    let fn_defs = parse_file(
        ExprParser::parse(parser::Rule::file, &f)
            .context("Failed to parse function definitions")?,
    );

    check_valid_program(&fn_defs)?;

    let executor = Executor::new(fn_defs, &args.max_iterations);
    let (paths, num_failed_observe_paths) = executor.run(args.prob)?;

		let num_samples = paths.iter().map(|p| p.num_uniform_samples + p.num_normal_samples).max().unwrap();
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
    } else {
        if let Some(output_path) = args.output {
            let mut f = File::create(output_path)?;
            writeln!(f, "Number of Paths: {}", paths.len())?;
            writeln!(f, "Number of Removed Paths: {num_failed_observe_paths}")?;
						writeln!(f, "Number of Samples: {num_samples}")?;
            for (i, path) in paths.iter().enumerate() {
                writeln!(f, "Path {}:\n\t{}", i + 1, path)?;
            }
            f.flush()?;
        }
        println!("Number of Paths: {}", paths.len());
        println!("Number of Removed Paths: {num_failed_observe_paths}");
				println!("Number of Samples: {num_samples}");
        for (i, path) in paths.iter().enumerate() {
            println!("Path {}:\n\t{}", i + 1, path);
        }
    }
    Ok(())
}
