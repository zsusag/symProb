use std::fmt::Display;

use crate::expr::Expr;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Path {
    conds: Vec<Expr>,
}

impl Path {
    pub fn new() -> Self {
        Path { conds: Vec::new() }
    }

    pub fn branch(&mut self, cond: Expr) {
        self.conds.push(cond)
    }

    pub fn get_conds(&self) -> &Vec<Expr> {
        &self.conds
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.conds
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(" ∧ ")
        )
    }
}
