use std::{collections::HashMap, io::Write, process::Command};

use anyhow::{Context, Result};
use tempfile::{Builder, NamedTempFile};

use crate::{executor_state::SymType, path::Path};

#[derive(Debug)]
pub struct Prob {
    temp: String,
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

        println!("stderr: {}", String::from_utf8(output.stderr)?);
        let prob = Prob {
            temp: String::from_utf8(output.stdout)?,
        };

        // Next step is to parse the distribution returned by Psi into an internal representation.

        f.close()?;
        // Run psi and get result back...
        Ok(prob)
    }
}
