use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::Display,
};

use anyhow::Result;
use itertools::Itertools;
use serde::Serialize;

use crate::{
    executor_state::SymType,
    expr::{Expr, ExprNode},
    probability::Prob,
};

/// A substitution \sigma, or a mapping from program variables to symbolic expressions.
///
/// \sigma captures the state of a symbolic trace. Each in-scope program variable `x` should map to
/// a symbolic expression representing `x`'s value at that moment in time.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Sigma(HashMap<String, Expr>);

impl Sigma {
    /// Creates a new, empty substitution.
    pub fn new() -> Self {
        Sigma(HashMap::new())
    }

    /// Returns a reference to `var`'s symbolic expression within the substitution, if
    /// found. Otherwise, returns `None`.
    pub fn get(&self, var: &str) -> Option<&Expr> {
        self.0.get(var)
    }

    /// Merge another substitution into this one.
    ///
    /// Inserts all of the entries in `other` into `self` and replaces expressions with existing
    /// program variables with the new expressions from `other`.
    pub fn merge(&mut self, other: Sigma) {
        self.0.extend(other.0)
    }
}

impl FromIterator<(String, Expr)> for Sigma {
    fn from_iter<T: IntoIterator<Item = (String, Expr)>>(iter: T) -> Self {
        Sigma(HashMap::from_iter(iter))
    }
}

/// A substitution is printed in alphabetic order by program variable.
impl Display for Sigma {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                // Sort the substitution by program variable.
                .sorted_by_key(|mapping| mapping.0)
                .format_with("; ", |(var, expr), g| g(&format_args!("σ({var}) = {expr}")))
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Path {
    conds: Vec<Expr>,
    path_prob: Option<Prob>,
    observes_prob: Option<Prob>,
    terminated: bool,
    sigma: Sigma,
    observations: Vec<Expr>,
    pub num_uniform_samples: u32,
    pub num_normal_samples: u32,
}

#[derive(Serialize)]
pub struct Row {
    path_num: usize,
    terminated: bool,
    num_uniform_samples: u32,
    num_normal_samples: u32,
    pc: String,
    po: String,
    pr_pc: Option<String>,
    pr_po: Option<String>,
    sigma: Vec<Option<String>>,
}

pub fn gen_csv_header(paths: &HashSet<Path>) -> (Vec<String>, BTreeSet<&String>) {
    let all_var_names: BTreeSet<&String> = paths.iter().flat_map(|p| p.sigma.keys()).collect();

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

impl Path {
    pub fn new() -> Self {
        Path {
            conds: Vec::new(),
            path_prob: None,
            observes_prob: None,
            terminated: false,
            sigma: Sigma::new(),
            observations: Vec::new(),
            num_uniform_samples: 0,
            num_normal_samples: 0,
        }
    }

    pub fn set_num_samples(&mut self, num_uniform_samples: u32, num_normal_samples: u32) {
        self.num_uniform_samples = num_uniform_samples;
        self.num_normal_samples = num_normal_samples;
    }

    pub fn branch(&mut self, mut cond: Expr, sigma: &HashMap<String, ExprNode>) {
        cond.simplify();
        self.conds.push(cond);
        self.merge_sigma(sigma);
    }

    pub fn observe(&mut self, mut observation: Expr) {
        observation.simplify();
        self.observations.push(observation);
    }

    pub fn get_path_observation(&self) -> &[Expr] {
        &self.observations
    }

    pub fn merge_sigma(&mut self, sigma: &HashMap<String, ExprNode>) {
        self.sigma
            .extend(sigma.iter().map(|(k, v)| (k.clone(), v.clone())));
    }

    pub fn simplify_sigma(&mut self) {
        self.sigma.values_mut().for_each(|val| val.simplify())
    }

    pub fn get_conds(&self) -> &Vec<Expr> {
        &self.conds
    }

    pub fn calculate_prob(&mut self, sym_vars: &HashMap<String, SymType>) -> Result<()> {
        self.path_prob = Some(Prob::new(&self.conds, sym_vars)?);
        self.observes_prob = Some(Prob::new(&self.observations, sym_vars)?);
        Ok(())
    }

    pub fn mark_terminated(&mut self) {
        self.terminated = true;
    }

    pub fn to_csv_row(&self, i: usize, all_var_names: &BTreeSet<&String>) -> Row {
        Row {
            path_num: i,
            terminated: self.terminated,
            num_uniform_samples: self.num_uniform_samples,
            num_normal_samples: self.num_normal_samples,
            pc: self
                .conds
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(" ∧ "),
            po: self
                .observations
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(" ∧ "),
            pr_pc: self.path_prob.as_ref().map(|p| p.to_string()),
            pr_po: self.observes_prob.as_ref().map(|p| p.to_string()),
            sigma: all_var_names
                .iter()
                .map(|var| self.sigma.get(*var).map(|val| val.to_string()))
                .collect(),
        }
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sigma_str = self
            .sigma
            .iter()
            .map(|(k, v)| format!("σ({k}) = {v}"))
            .collect::<Vec<String>>()
            .join(", ");
        match (&self.path_prob, &self.observes_prob) {
            (None, None) => {
                if self.terminated {
                    write!(
                        f,
                        "Path Condition: {}\n\tObservations: {}\n\tSigma: {}\n\tForced termination: Yes",
                        self.conds
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<String>>()
                            .join(" ∧ "),
                        self.observations
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<String>>()
                            .join(" ∧ "),
                        sigma_str
                    )
                } else {
                    write!(
                        f,
                        "Path Condition: {}\n\tObservations: {}\n\tSigma: {}\n\tForced termination: No",
                        self.conds
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<String>>()
                            .join(" ∧ "),
                        self.observations
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<String>>()
                            .join(" ∧ "),
                        sigma_str
                    )
                }
            }
            (Some(path_prob), Some(observe_prob)) => {
                if self.terminated {
                    write!(
                f,
                "Path Condition: {}\n\tProbability: {}\n\tObservations: {}\n\tObservations Probability: {}\n\tSigma: {}\n\tForced termination: Yes",
                self.conds
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(" ∧ "),
                path_prob,
                self.observations
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(" ∧ "),
								observe_prob,
                sigma_str
            )
                } else {
                    write!(
                f,
                "Path Condition: {}\n\tProbability: {}\n\tObservations: {}\n\tObservations Probability: {}\n\tSigma: {}\n\tForced termination: No",
                self.conds
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(" ∧ "),
                path_prob,
                self.observations
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(" ∧ "),
								observe_prob,
                sigma_str
            )
                }
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax::{ExprKind, Value};

    use super::*;

    #[test]
    fn sigma_display() {
        let sigma = Sigma::from_iter([
            (
                "y".to_string(),
                Expr::new(ExprNode::new_leaf(ExprKind::Constant(Value::Boolean(true)))),
            ),
            (
                "x".to_string(),
                Expr::new(ExprNode::new_leaf(ExprKind::Constant(Value::Boolean(true)))),
            ),
            (
                "z".to_string(),
                Expr::new(ExprNode::new_leaf(ExprKind::Constant(Value::Boolean(true)))),
            ),
        ]);

        let expected = "σ(x) = true; σ(y) = true; σ(z) = true";
        assert_eq!(format!("{sigma}"), expected);
    }
}
