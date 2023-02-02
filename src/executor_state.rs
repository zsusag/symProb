use std::collections::HashMap;

use anyhow::Result;
use num::Rational32;

use crate::{
    expr::{Expr, ExprNode},
    path::Path,
    probability::Prob,
    smt::SMTMangager,
    syntax::{ExprKind, Func, Statement, StatementKind, Type, Value},
};

// Add bernoulli sampling. normal, etc.
#[derive(Debug)]
pub enum Status {
    Fork(ExecutorState, ExecutorState),
    Continue(ExecutorState),
    Terminate(Path),
    Return(Expr),
    FailedObserve,
    PrematureTerminate,
}

#[derive(Debug, Clone)]
pub enum SymType {
    Normal(Type),
    UniformProb,
    NormalProb,
}

impl SymType {
    pub fn is_prob(&self) -> bool {
        !matches!(self, SymType::Normal(_))
    }
}

#[derive(Debug, Clone)]
pub struct ScopeState {
    id: u32,
    sigma: HashMap<String, ExprNode>,
}

impl ScopeState {
    pub fn new(id: u32, sigma: HashMap<String, ExprNode>) -> Self {
        ScopeState { id, sigma }
    }

    pub fn comp_id(&self, id: u32) -> bool {
        self.id == id
    }

    pub fn restore(self) -> HashMap<String, ExprNode> {
        self.sigma
    }
}

#[derive(Debug, Clone)]
pub struct ExecutorState {
    // Stack of statements to still be executed.
    stack: Vec<Statement>,

    // Substitution map from program variables to symbolic expressions
    sigma: HashMap<String, ExprNode>,

    // List of path constraints
    path: Path,

    // Mapping of symbolic variables
    sym_vars: HashMap<String, SymType>,

    // Counter for number of uniform probabilistic samples
    num_uniform_samples: u32,

    // Counter for number of normal probabilistic samples
    num_normal_samples: u32,

    // A stack representing the scopes present
    scope_manager: Vec<ScopeState>,

    // The SMT manager which holds the Z3 context and configuration
    smt_manager: SMTMangager,

    // A mapping from while-loop identifiers to the previous path condition for almost-sure-termination
    prev_iter_map: HashMap<u32, Path>,

    max_iterations: Option<u32>,
    iter_map: HashMap<u32, u32>,
}

impl ExecutorState {
    pub fn new(main_fn: Func, max_iterations: &Option<u32>) -> Self {
        let Func {
            inputs, mut body, ..
        } = main_fn;

        let (input_names, input_types): (Vec<_>, Vec<_>) = inputs.into_iter().unzip();
        let sym_vars = input_names
            .iter()
            .zip(input_types.into_iter())
            .map(|(x, t)| (x.clone(), SymType::Normal(t)))
            .collect();

        let sigma = input_names
            .into_iter()
            .map(|x| {
                (
                    x.clone(),
                    ExprNode::new_leaf(ExprKind::Constant(Value::Var(x))),
                )
            })
            .collect();

        // Reverse the statements as push/pop do so from the end of the Vec
        body.reverse();

        ExecutorState {
            stack: body,
            sigma,
            path: Path::new(),
            sym_vars,
            num_uniform_samples: 0,
            num_normal_samples: 0,
            scope_manager: Vec::new(),
            smt_manager: SMTMangager::new(),
            prev_iter_map: HashMap::new(),
            max_iterations: max_iterations.clone(),
            iter_map: HashMap::new(),
        }
    }

    pub fn uniform_sample(&mut self) -> String {
        // Generate name for the probabilistic symbolic variable which is the result from sampling
        let prob_sym_name = format!("UNIFORM_{:}", self.num_uniform_samples);

        // Update the symbolic variable type map
        self.sym_vars
            .insert(prob_sym_name.clone(), SymType::UniformProb);

        // Increment the number of samples
        self.num_uniform_samples += 1;
        prob_sym_name
    }

    pub fn normal_sample(&mut self) -> String {
        // Generate name for the probabilistic symbolic variable which is the result from sampling
        let prob_sym_name = format!("NORMAL_{:}", self.num_normal_samples);

        // Update the symbolic variable type map
        self.sym_vars
            .insert(prob_sym_name.clone(), SymType::NormalProb);

        // Increment the number of samples
        self.num_normal_samples += 1;
        prob_sym_name
    }

    pub fn check_scope(&mut self, statement: &Statement) {
        if let Some(ss) = self.scope_manager.pop() {
            if ss.comp_id(statement.get_id()) {
                // The current sigma should be modified to drop any variables initialized in the inner scope
                let old_sigma = ss.restore();

                // Remove all entries in sigma that didn't exist before the inner scope was introduced
                self.sigma.retain(|k, _| old_sigma.contains_key(k));
            } else {
                // Else, push the scope state back on the stack
                self.scope_manager.push(ss);
            }
        }
    }
    // Need to add back the while statement on true branch side
    pub fn fork(mut self, inner_body: Vec<Statement>, guard: Option<Expr>) -> Self {
        if !inner_body.is_empty() {
            // First save the current scope state, if there are statements to execute after the scope
            if let Some(next_st) = self.stack.last() {
                // Grab the id of the next statement to be executed AFTER the inner_body has finished executing
                let exit_scope_id = next_st.get_id();
                self.scope_manager
                    .push(ScopeState::new(exit_scope_id, self.sigma.clone()))
            }
        }

        // Add the new condition to the current path, if not trivial
        if let Some(e) = guard {
            self.path.branch(e, &self.sigma);
        }

        // Add the inner scope statements to the stack
        for s in inner_body.into_iter().rev() {
            self.stack.push(s);
        }
        self
    }

    // Going to ignore function/macro calls for now.
    pub fn step(mut self, no_prob: bool) -> Result<Status> {
        let s = self.stack.pop();
        match s {
            Some(s) => {
                // First need to check whether we need to restore the state
                self.check_scope(&s);
                let s_id = s.get_id();
                match s.kind {
                    StatementKind::Assignment(var, e) => {
                        // Apply substitution map to expression to get new value for the variable...
                        let mut val = e.substitute_and_get_root(&self.sigma);

                        // Simplify
                        val.simplify();

                        self.sigma.insert(var, val);
                        Ok(Status::Continue(self))
                    }
                    StatementKind::Sample(var) => {
                        let sym_name = self.uniform_sample();
                        self.sigma.insert(var, ExprNode::new_sample_var(sym_name));
                        Ok(Status::Continue(self))
                    }
                    StatementKind::Bernoulli(var, e) => {
                        // We implement Bernoulli sampling by pushing a series of statements to the stack.
                        let var_set_zero = Statement::new_with_id(
                            StatementKind::Assignment(
                                var.clone(),
                                Expr::new(ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                                    Rational32::new(0, 1),
                                )))),
                            ),
                            s_id + 3,
                        );
                        let var_set_one = Statement::new_with_id(
                            StatementKind::Assignment(
                                var.clone(),
                                Expr::new(ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                                    Rational32::new(1, 1),
                                )))),
                            ),
                            s_id + 2,
                        );
                        let cond = Expr::new(ExprNode::new(
                            ExprKind::Lt,
                            vec![
                                ExprNode::new_leaf(ExprKind::Constant(Value::Var(var.clone()))),
                                e.get_root(),
                            ],
                        ));
                        let branch = Statement::new_with_id(
                            StatementKind::Branch(cond, vec![var_set_one], vec![var_set_zero]),
                            s_id + 1,
                        );
                        self.stack.push(branch);
                        let sample = Statement::new_with_id(StatementKind::Sample(var), s_id);
                        self.stack.push(sample);
                        Ok(Status::Continue(self))
                    }
                    StatementKind::Normal(var, mean, variance) => {
                        let mut mean = mean.get_root();
                        let mut variance = variance.get_root();

                        // Apply sigma to both mean and variance
                        mean.substitute(&self.sigma);
                        variance.substitute(&self.sigma);

                        let sym_name = self.normal_sample();
                        if mean
                            == ExprNode::new_leaf(ExprKind::Constant(Value::Num(Rational32::new(
                                0, 1,
                            ))))
                            && variance
                                == ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                                    Rational32::new(1, 1),
                                )))
                        {
                            // Standard normal distribution
                            self.sigma.insert(var, ExprNode::new_sample_var(sym_name));
                        } else {
                            // Need to transform
                            let sqrt_variance = ExprNode::new(ExprKind::Sqrt, vec![variance]);
                            let mult_by_variance = ExprNode::new(
                                ExprKind::Mul,
                                vec![ExprNode::new_sample_var(sym_name), sqrt_variance],
                            );
                            let shift_by_mean =
                                ExprNode::new(ExprKind::Add, vec![mean, mult_by_variance]);
                            self.sigma.insert(var, shift_by_mean);
                        }
                        Ok(Status::Continue(self))
                    }
                    StatementKind::Uniform(var, lower, upper) => {
                        let mut lower = lower.get_root();
                        let mut upper = upper.get_root();

                        // Apply sigma to both lower and upper expressions
                        lower.substitute(&self.sigma);
                        upper.substitute(&self.sigma);

                        let sym_name = self.uniform_sample();
                        if lower
                            == ExprNode::new_leaf(ExprKind::Constant(Value::Num(Rational32::new(
                                0, 1,
                            ))))
                            && upper
                                == ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                                    Rational32::new(1, 1),
                                )))
                        {
                            self.sigma.insert(var, ExprNode::new_sample_var(sym_name));
                        } else {
                            let scale_factor =
                                ExprNode::new(ExprKind::Sub, vec![upper, lower.clone()]);
                            let mult_sample_by_scale = ExprNode::new(
                                ExprKind::Mul,
                                vec![ExprNode::new_sample_var(sym_name), scale_factor],
                            );
                            let final_shift =
                                ExprNode::new(ExprKind::Add, vec![mult_sample_by_scale, lower]);
                            self.sigma.insert(var, final_shift);
                        }
                        Ok(Status::Continue(self))
                    }
                    StatementKind::Branch(mut guard, tru_branch, fls_branch) => {
                        // Apply sigma on the branch guard
                        guard.substitute(&self.sigma);

                        // Call out to SMT solver first, check to see if either side is satisfiable
                        let (true_sat, false_sat) = self.smt_manager.check_fork_satisfiability(
                            self.path.get_conds(),
                            &guard,
                            &self.sym_vars,
                        );

                        let status = match (true_sat, false_sat) {
                            (true, true) => Status::Fork(
                                self.clone().fork(tru_branch, Some(guard.clone())),
                                self.fork(fls_branch, Some(guard.not())),
                            ),
                            (true, false) => Status::Continue(self.fork(tru_branch, None)),
                            (false, true) => Status::Continue(self.fork(fls_branch, None)),
                            (false, false) => Status::PrematureTerminate,
                        };
                        Ok(status)
                    }
                    StatementKind::While(guard, body) => {
                        // Apply sigma on the branch guard
                        let (true_sat, false_sat) = self.smt_manager.check_fork_satisfiability(
                            self.path.get_conds(),
                            &guard.clone_and_substitute(&self.sigma),
                            &self.sym_vars,
                        );

                        let iters_so_far = self.iter_map.get_mut(&s_id);

                        if true_sat {
                            if let Some(max_iters) = self.max_iterations {
                                if let Some(num_iters_so_far) = &iters_so_far {
                                    if **num_iters_so_far >= max_iters {
                                        // We have reached the maximum number of iterations allowed
                                        self.path.mark_terminated();
                                        if false_sat {
                                            return Ok(Status::Continue(
                                                self.fork(Vec::new(), Some(guard.not())),
                                            ));
                                        } else {
                                            return Ok(Status::Continue(self));
                                        }
                                    }
                                }
                            }

                            // if let Some(prev_iter_path) = self.prev_iter_map.get(&s_id) {
                            //     // Visited this loop before; check for almost-surely-terminating loop...
                            //     if Prob::is_almost_surely_terminating(
                            //         prev_iter_path,
                            //         &self.path,
                            //         &self.sym_vars,
                            //     )? {
                            //         // Don't enter the loop!
                            //         self.path.mark_terminated();
                            //         if false_sat {
                            //             return Ok(Status::Continue(
                            //                 self.fork(Vec::new(), Some(guard.not())),
                            //             ));
                            //         } else {
                            //             return Ok(Status::Continue(self));
                            //         }
                            //     }
                            // }
                            // Insert the while loop into the prev iteration, unless it is almost-surely-terminating.
                            //                            self.prev_iter_map.insert(s_id, self.path.clone());

                            // Increment the number of iterations
                            match iters_so_far {
                                Some(iters_so_far) => *iters_so_far += 1,
                                None => {
                                    self.iter_map.insert(s_id, 1);
                                }
                            };

                            if false_sat {
                                let mut into_loop_state = self.clone();
                                // Push copy of while loop onto stack before forking
                                into_loop_state.stack.push(Statement::clone_while(
                                    guard.clone(),
                                    body.clone(),
                                    s_id,
                                ));
                                Ok(Status::Fork(
                                    into_loop_state.fork(body, Some(guard.clone())),
                                    self.fork(Vec::new(), Some(guard.not())),
                                ))
                            } else {
                                self.stack
                                    .push(Statement::clone_while(guard, body.clone(), s_id));
                                Ok(Status::Continue(self.fork(body, None)))
                            }
                        } else if false_sat {
                            Ok(Status::Continue(self))
                        } else {
                            Ok(Status::PrematureTerminate)
                        }
                    }
                    StatementKind::Return(_) => todo!(),
                    StatementKind::Observe(mut cond) => {
                        cond.substitute(&self.sigma);
                        self.path.observe(cond);
                        // Check whether observe statement has 0 probability (i.e., unsatisfiable)
                        if self
                            .smt_manager
                            .is_sat(self.path.get_path_observation(), &self.sym_vars)
                        {
                            // Non-zero probability
                            Ok(Status::Continue(self))
                        } else {
                            // Zero probability
                            Ok(Status::FailedObserve)
                        }
                    }
                }
            }
            None => {
                self.path.merge_sigma(&self.sigma);
                self.path.simplify_sigma();
                if !no_prob {
                    self.path.calculate_prob(&self.sym_vars)?;
                }
                Ok(Status::Terminate(self.path))
            }
        }
    }
}
