use std::{
    collections::{BTreeSet, HashMap},
    fmt::Display,
};

use anyhow::Result;
use itertools::Itertools;
use serde::Serialize;

use crate::{
    executor_state::SymType,
    expr::{Expr, PostExpectation},
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

    /// Returns `true` if and only if `var` is in the substitution.
    pub fn contains_var(&self, var: &str) -> bool {
        self.0.contains_key(var)
    }

    /// Returns a reference to `var`'s symbolic expression within the substitution, if
    /// found. Otherwise, returns `None`.
    pub fn get(&self, var: &str) -> Option<&Expr> {
        self.0.get(var)
    }

    /// Updates the substitution to map `var` to `expr`.
    ///
    /// The old expression which `var` mapped to, if it exists, is returned. Otherwise, `None` is
    /// returned.
    pub fn insert<S, E>(&mut self, var: S, expr: E) -> Option<Expr>
    where
        S: ToString,
        E: Into<Expr>,
    {
        self.0.insert(var.to_string(), expr.into())
    }

    /// Merge another substitution into this one.
    ///
    /// Inserts all of the entries in `other` into `self` and replaces expressions with existing
    /// program variables with the new expressions from `other`.
    #[allow(dead_code)]
    pub fn merge(&mut self, other: Sigma) {
        self.0.extend(other.0)
    }

    /// Updates the substitution by inserting all the entries in `other` into the
    /// substitution. Existing entries are overwritten with those in `other`.
    ///
    /// This method only clones when an entry in `other` either does not exist in `self` or does not
    /// equal the expression stored in `self`. In other words, [`update`] only clones when it must
    /// update the substitution.
    pub fn update(&mut self, other: &Sigma) {
        let updates: Vec<_> = other
            .0
            .iter()
            .filter(|(var, expr)| match self.get(var) {
                Some(cur_expr) => cur_expr != *expr,
                None => true,
            })
            .map(|(var, expr)| (var.clone(), expr.clone()))
            .collect();
        self.0.extend(updates)
    }

    /// Simplifies all of the expressions within the substitution.
    pub fn simplify(&mut self) {
        self.0.values_mut().for_each(|expr| expr.simplify())
    }

    /// Returns an iterator over borrowed references to the program variables in the substitution.
    pub fn variables(&self) -> impl Iterator<Item = &String> {
        self.0.keys()
    }

    /// Removes out-of-scope variables from the substitution.
    ///
    /// Only the variables in `scope` will remain in the substitution after this method has
    /// terminated. In other words, all variables not in `scope` will be removed from the
    /// substitution.
    pub fn remove_out_of_scope_vars(&mut self, scope: &Sigma) {
        self.0.retain(|var, _| scope.contains_var(var))
    }

    /// Applies the substitution on `expr`.
    pub fn apply(&self, expr: &mut Expr) {
        expr.substitute(self);
        expr.simplify();
    }

    /// An iterator visiting all substitution pairs in an arbitrary order. The iterator element type
    /// is `(&'a String, &'a Expr)`.
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a String, &'a Expr)> {
        self.0.iter()
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

#[derive(Debug, Clone)]
pub struct Path {
    conds: Vec<Expr>,
    path_prob: Option<Prob>,
    observes_prob: Option<Prob>,
    terminated: bool,
    sigma: Sigma,
    observations: Vec<Expr>,
    postexpectation: Option<PostExpectation>,
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

pub fn gen_csv_header<'a, I>(paths: I) -> (Vec<String>, BTreeSet<&'a String>)
where
    I: IntoIterator<Item = &'a Path>,
{
    let all_var_names: BTreeSet<_> = paths
        .into_iter()
        .flat_map(|p| p.sigma.variables())
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

impl Path {
    pub fn new() -> Self {
        Path {
            conds: Vec::new(),
            path_prob: None,
            observes_prob: None,
            terminated: false,
            sigma: Sigma::new(),
            observations: Vec::new(),
            postexpectation: None,
            num_uniform_samples: 0,
            num_normal_samples: 0,
        }
    }

    pub fn set_num_samples(&mut self, num_uniform_samples: u32, num_normal_samples: u32) {
        self.num_uniform_samples = num_uniform_samples;
        self.num_normal_samples = num_normal_samples;
    }

    pub fn branch(&mut self, mut cond: Expr, sigma: &Sigma) {
        cond.simplify();
        self.conds.push(cond);
        self.sigma.update(sigma);
    }

    pub fn observe(&mut self, mut observation: Expr) {
        observation.simplify();
        self.observations.push(observation);
    }

    pub fn get_path_observation(&self) -> &[Expr] {
        &self.observations
    }

    pub fn update_sigma(&mut self, sigma: &Sigma) {
        self.sigma.update(sigma)
    }

    pub fn simplify_sigma(&mut self) {
        self.sigma.simplify()
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

    pub fn get_sigma(&self) -> &Sigma {
        &self.sigma
    }

    /// Adds a postexpectation to the path and applies the path's substitution map to the
    /// postexpectation.
    pub fn add_postexpectation(&mut self, mut post: PostExpectation) {
        post.apply_sigma(&self.sigma);
        self.postexpectation = Some(post);
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
                        self.sigma
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
                        self.sigma
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
                self.sigma
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
                self.sigma
            )
                }
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        expr::ExprNode,
        syntax::{ExprKind, Value},
    };

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
