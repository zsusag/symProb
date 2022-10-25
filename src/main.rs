use anyhow::{Context, Result};
use clap::Parser;
use num::rational::Ratio;
use pest::Parser as EParser;
use std::fs;

use crate::{
    expr::{Expr, ExprNode},
    parser::{parse_expr, parse_file, ExprParser},
    syntax::{ExprKind, Value},
};

mod expr;
mod parser;
mod syntax;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// path to probabilistic programs
    p: std::path::PathBuf,
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

    println!("{:#?}", fn_defs);

    Ok(())
}
