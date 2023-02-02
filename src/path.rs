use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

use anyhow::Result;

use crate::{
    executor_state::SymType,
    expr::{Expr, ExprNode},
    probability::Prob,
};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Path {
    conds: Vec<Expr>,
    path_prob: Option<Prob>,
    observes_prob: Option<Prob>,
    terminated: bool,
    sigma: BTreeMap<String, ExprNode>,
    observations: Vec<Expr>,
}

impl Path {
    pub fn new() -> Self {
        Path {
            conds: Vec::new(),
            path_prob: None,
            observes_prob: None,
            terminated: false,
            sigma: BTreeMap::new(),
            observations: Vec::new(),
        }
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
            .extend(sigma.into_iter().map(|(k, v)| (k.clone(), v.clone())));
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
                        "Path Condition: {}\n\tObservations: {}\n\tSigma: {}\n\tTerminated: Yes",
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
                        "Path Condition: {}\n\tObservations: {}\n\tSigma: {}\n\tTerminated: No",
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
                "Path Condition: {}\n\tProbability: {}\n\tObservations: {}\n\tObservations Probability: {}\n\tSigma: {}\n\tTerminated: Yes",
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
                "Path Condition: {}\n\tProbability: {}\n\tObservations: {}\n\tObservations Probability: {}\n\tSigma: {}\n\tTerminated: No",
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
