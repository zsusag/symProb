use std::{collections::HashMap, fmt::Display};

use anyhow::Result;

use crate::{executor_state::SymType, expr::Expr, probability::Prob};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Path {
    conds: Vec<Expr>,
    prob: Prob,
    terminated: bool,
}

impl Path {
    pub fn new() -> Self {
        Path {
            conds: Vec::new(),
            prob: Prob::init_dist(),
            terminated: false,
        }
    }

    pub fn branch(&mut self, cond: Expr) {
        self.conds.push(cond)
    }

    pub fn get_conds(&self) -> &Vec<Expr> {
        &self.conds
    }

    pub fn calculate_prob(&mut self, sym_vars: &HashMap<String, SymType>) -> Result<()> {
        self.prob = Prob::new(self, sym_vars)?;
        Ok(())
    }

    pub fn mark_terminated(&mut self) {
        self.terminated = true;
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.terminated {
            write!(
                f,
                "Path Condition: {}, Probability: {}, Terminated",
                self.conds
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(" ∧ "),
                self.prob
            )
        } else {
            write!(
                f,
                "Path Condition: {}, Probability: {}",
                self.conds
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(" ∧ "),
                self.prob
            )
        }
    }
}
