use std::{collections::HashMap, fmt::Display};

use anyhow::Result;
use itertools::Itertools;
use num::Rational32;
use serde::Serialize;

use crate::{
    expr::{Expr, PostExpectation, PreExpectationIntegrand},
    probability::Prob,
    python::PyPathPreExpectation,
    symbolic::{Dist, SymVarMap},
};

/// A substitution \sigma, or a mapping from program variables to symbolic expressions.
///
/// \sigma captures the state of a symbolic trace. Each in-scope program variable `x` should map to
/// a symbolic expression representing `x`'s value at that moment in time.
#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(transparent)]
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
    pub fn iter(&self) -> impl Iterator<Item = (&'_ String, &Expr)> {
        self.0.iter()
    }
}

impl Default for Sigma {
    fn default() -> Self {
        Self::new()
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

#[derive(Debug, Clone, Serialize)]
#[serde(into = "SerdePath")]
pub struct Path {
    conds: Vec<Expr>,
    path_prob: Option<Prob>,
    observes_prob: Option<Prob>,
    forcibly_terminated: bool,
    sigma: Sigma,
    observations: Vec<Expr>,
    postexpectation: Option<PostExpectation>,
    pub psvs: HashMap<String, Dist>,
    pub num_uniform_samples: u32,
    pub num_normal_samples: u32,
}

impl Path {
    pub fn new() -> Self {
        Path {
            conds: Vec::new(),
            path_prob: None,
            observes_prob: None,
            forcibly_terminated: false,
            sigma: Sigma::new(),
            observations: Vec::new(),
            postexpectation: None,
            psvs: HashMap::new(),
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

    pub fn calculate_prob(&mut self, sym_vars: &SymVarMap) -> Result<()> {
        self.path_prob = Some(Prob::new(&self.conds, sym_vars)?);
        self.observes_prob = Some(Prob::new(&self.observations, sym_vars)?);
        Ok(())
    }

    pub fn forcibly_terminated(&self) -> bool {
        self.forcibly_terminated
    }

    pub fn mark_forcibly_terminated(&mut self) {
        self.forcibly_terminated = true;
    }

    #[allow(dead_code)]
    pub fn get_sigma(&self) -> &Sigma {
        &self.sigma
    }

    /// Adds a postexpectation to the path and applies the path's substitution map to the
    /// postexpectation. If the postexpectation is ill-typed, then `Err(_)` is returned. Otherwise,
    /// `Ok(())` is returned.
    pub fn add_postexpectation(&mut self, mut post: PostExpectation) -> Result<()> {
        // Typecheck the postexpectation using the path's substitution, forwarding on any errors
        // that were encountered to the caller.
        post.typecheck(&self.sigma)?;
        post.apply_sigma(&self.sigma);
        self.postexpectation = Some(post);
        Ok(())
    }

    /// Returns the pre-expectation for the path. If the path does not have a postexpectation (i.e.,
    /// `self.postexpectation` is `None`), this method returns `None`.
    ///
    /// **Warning**: This method clones the path condition, path observation, and postexpectation
    /// thereby triggering memory allocations. If the path will no longer be needed, consider
    /// using [`Path::into_preexpectation`] instead.
    pub fn preexpectation(&self) -> Option<PreExpectationIntegrand> {
        self.postexpectation.clone().map(|post| {
            PreExpectationIntegrand::new(self.conds.clone(), self.observations.clone(), post)
        })
    }

    /// Returns the integrand for the normalization constant of the pre-expectation for the path.
    pub fn preexp_normal_const(&self) -> Option<PreExpectationIntegrand> {
        let one: Expr = Rational32::ONE.into();
        if self.postexpectation.is_some() {
            Some(PreExpectationIntegrand::new(
                self.conds.clone(),
                self.observations.clone(),
                one.into(),
            ))
        } else {
            None
        }
    }

    /// Returns the pre-expectation for the path by consuming the path. If the path does not have a postexpectation (i.e.,
    /// `self.postexpectation` is `None`), this method returns `None`.
    ///
    /// **Warning**: This method clones the path condition, path observation, and postexpectation
    /// thereby triggering memory allocations. If the path will no longer be needed, consider
    /// using [`Path::into_preexpectation`] instead.
    #[allow(dead_code)]
    pub fn into_preexpectation(self) -> Option<PreExpectationIntegrand> {
        let Path {
            conds,
            observations,
            postexpectation,
            ..
        } = self;
        postexpectation.map(|post| PreExpectationIntegrand::new(conds, observations, post))
    }

    pub fn integrable_preexpectation(&self) -> Option<PyPathPreExpectation> {
        self.preexpectation()
            .map(|integrand| PyPathPreExpectation::new(integrand, self.psvs.iter()))
    }

    pub fn integrable_preexp_normal_const(&self) -> Option<PyPathPreExpectation> {
        self.preexp_normal_const()
            .map(|integrand| PyPathPreExpectation::new(integrand, self.psvs.iter()))
    }
}

/// Paths are displayed in a block indented by a single TAB character (i.e., `'\t'`).
impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Print the path condition as the conjunction of the individual path conjuncts.
        writeln!(f, "\tPath Condition: {}", self.conds.iter().format(" ∧ "))?;

        // If a path probability has been computed, print it.
        if let Some(path_prob) = &self.path_prob {
            writeln!(f, "\tProbability: {path_prob}")?;
        }

        // Print the path observations as the conjunction of the individual observations.
        writeln!(
            f,
            "\tObservations: {}",
            self.observations.iter().format(" ∧ ")
        )?;

        // If the probability of observing the path observations have bene computed, print it.
        if let Some(observes_prob) = &self.observes_prob {
            writeln!(f, "\tObservations Probability: {observes_prob}")?;
        }

        // Print the substitution map [`Sigma`].
        writeln!(f, "\tSubstitution (σ): {}", self.sigma)?;

        // Print whether the path was forced to terminate.
        writeln!(
            f,
            "\tForcibly terminated: {}",
            if self.forcibly_terminated {
                "Yes"
            } else {
                "No"
            }
        )
    }
}

#[derive(Serialize)]
#[serde(rename = "path")]
pub struct SerdePath {
    condition: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    condition_probability: Option<Prob>,
    observations: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    observations_probability: Option<Prob>,
    sigma: Sigma,
    forcibly_terminated: bool,
    uniform_samples: u32,
    normal_samples: u32,
}

impl From<Path> for SerdePath {
    fn from(p: Path) -> Self {
        let Path {
            conds,
            path_prob,
            observes_prob,
            forcibly_terminated,
            sigma,
            observations,
            num_uniform_samples,
            num_normal_samples,
            ..
        } = p;
        SerdePath {
            condition: conds.iter().join(" ∧ "),
            condition_probability: path_prob,
            observations: observations.iter().join(" ∧ "),
            observations_probability: observes_prob,
            sigma,
            forcibly_terminated,
            uniform_samples: num_uniform_samples,
            normal_samples: num_normal_samples,
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
