use std::{collections::HashMap, fmt::Display, io::Write, process::Command};

use anyhow::{Context, Result};
use pest::Parser as EParser;
use tempfile::{Builder, NamedTempFile};

use crate::{
    executor_state::SymType,
    expr::Expr,
    path::Path,
    psi_parser::{self, parse_psi_output, PsiParser},
};

#[derive(Debug)]
pub struct Prob {
    p: Expr,
}

impl Prob {
    pub fn new(path: &Path, sym_vars: &HashMap<String, SymType>) -> Result<Self> {
        // Make new tempfile to store intermediary PSI program in
        let mut f = Builder::new().suffix(".psi").rand_bytes(5).tempfile()?;

        // Write the `main` function header
        let (normal_sym_vars, prob_sym_vars): (Vec<_>, Vec<_>) =
            sym_vars.iter().partition(|(_, t)| match t {
                SymType::Normal(_) => true,
                SymType::Prob => false,
            });

        writeln!(
            f,
            "def main({}) {{",
            normal_sym_vars
                .iter()
                .map(|(name, _)| name.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )?;

        // Declare all of the probabilistic symbolic variables as samples from [0,1]
        for (name, _) in prob_sym_vars {
            writeln!(f, "{name} := uniform(0,1);")?;
        }

        for cond in path.get_conds() {
            writeln!(f, "assert({});", cond.to_psi_expr())?;
        }
        // Write closing curly brace to main function
        writeln!(f, "}}")?;

        let output = Command::new("psi")
            .arg(f.path().as_os_str())
            .output()
            .context("unable to call psi")?;

        println!("stdout: {}", String::from_utf8(output.stdout.clone())?);
        let prob = Prob {
            p: parse_psi_output(
                PsiParser::parse(
                    psi_parser::Rule::psi_prob,
                    &String::from_utf8(output.stdout)?,
                )
                .context("Failed to parse Psi distribution")?,
            ),
        };

        f.close()?;
        // Run psi and get result back...
        Ok(prob)
    }
}

impl Display for Prob {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.p)
    }
}
