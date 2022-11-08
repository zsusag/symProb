use std::{collections::HashMap, fmt::Display, io::Write, process::Command};

use anyhow::{bail, Context, Result};
use num::Rational32;
use pest::Parser as EParser;
use tempfile::{Builder, NamedTempFile};

use crate::{
    executor_state::SymType,
    expr::{Expr, ExprNode},
    path::Path,
    psi_parser::{self, parse_psi_output, PsiParser},
    syntax::{ExprKind, Value},
};

struct PsiProg(NamedTempFile);

impl PsiProg {
    fn new(sym_vars: &HashMap<String, SymType>) -> Result<Self> {
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
        Ok(PsiProg(f))
    }

    fn write_assertions(&mut self, path: &Path) -> Result<()> {
        for cond in path.get_conds() {
            writeln!(self.0, "assert({});", cond.to_psi_expr())
                .context("Couldn't write assertion to Psi program tempfile")?;
        }
        Ok(())
    }

    fn write_observes(&mut self, path: &Path) -> Result<()> {
        for cond in path.get_conds() {
            writeln!(self.0, "observe({});", cond.to_psi_expr())
                .context("Couldn't write assertion to Psi program tempfile")?;
        }
        Ok(())
    }

    fn run(mut self) -> Result<Prob> {
        // Write the final closing curly bracket right before running
        writeln!(self.0, "}}")?;

        let output = Command::new("psi")
            .arg(self.0.path().as_os_str())
            .output()
            .context("Unable to call Psi (is it on your path?)")?;

        // Close the file after running Psi.
        self.0.close().context("Error closing Psi program file")?;

        // Check to see if Psi encountered an error, and if so, return the contents of stderr
        if !output.status.success() {
            bail!(
                "Psi encountered an error:\n{}",
                String::from_utf8(output.stderr)
                    .context("Unable to encode Psi output as a UTF-8 string.")?
            )
        }

        Ok(Prob(parse_psi_output(
            PsiParser::parse(
                psi_parser::Rule::psi_prob,
                &String::from_utf8(output.stdout)?,
            )
            .context("Failed to parse Psi distribution")?,
        )))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Prob(Expr);

impl Prob {
    pub fn new(path: &Path, sym_vars: &HashMap<String, SymType>) -> Result<Self> {
        // Make a new Psi program
        let mut pp = PsiProg::new(sym_vars)?;

        // Write the path condition as an assertion in the Psi program
        pp.write_assertions(path)?;

        // Run Psi to obtain `path`'s probability
        pp.run()
    }

    pub fn is_almost_surely_terminating(
        old_path: &Path,
        new_path: &Path,
        sym_vars: &HashMap<String, SymType>,
    ) -> Result<bool> {
        // Make a new Psi program
        let mut pp = PsiProg::new(sym_vars)?;

        // Write the old path condition as *observe* statements
        pp.write_observes(old_path)?;

        // Write the new path condition as *assert* statements
        pp.write_assertions(new_path)?;

        // Run Psi to return the conditional probability of the new path
        let cond_prob = pp.run()?;

        Ok(cond_prob.0.is_constant())
    }

    pub fn init_dist() -> Self {
        Prob(Expr::new(ExprNode::new_leaf(ExprKind::Constant(
            Value::Num(Rational32::new(1, 1)),
        ))))
    }
}

impl Display for Prob {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
