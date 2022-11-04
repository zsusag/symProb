use std::collections::HashMap;

use crate::{
    expr::{Expr, ExprNode},
    path::Path,
    smt::SMTMangager,
    syntax::{ExprKind, Func, Statement, StatementKind, Type, Value},
};

#[derive(Debug)]
pub enum Status {
    Fork(Vec<ExecutorState>),
    Continue(ExecutorState),
    Terminate(Path),
    Return(Expr),
}

#[derive(Debug, Clone)]
pub enum SymType {
    Normal(Type),
    Prob,
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

    // Counter for number of probabilistic samples
    num_samples: u32,

    // A stack representing the scopes present
    scope_manager: Vec<ScopeState>,

    // The SMT manager which holds the Z3 context and configuration
    smt_manager: SMTMangager,
}

impl ExecutorState {
    pub fn new(main_fn: Func) -> Self {
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
            num_samples: 0,
            scope_manager: Vec::new(),
            smt_manager: SMTMangager::new(),
        }
    }

    pub fn sample(&mut self) -> String {
        // Generate name for the probabilistic symbolic variable which is the result from sampling
        let prob_sym_name = format!("!SAMPLE_{:}", self.num_samples);

        // Update the symbolic variable type map
        self.sym_vars.insert(prob_sym_name.clone(), SymType::Prob);

        // Increment the number of samples
        self.num_samples += 1;
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

    pub fn fork(mut self, inner_body: Vec<Statement>, guard: Expr) -> Self {
        if !inner_body.is_empty() {
            // First save the current scope state, if there are statements to execute after the scope
            if let Some(next_st) = self.stack.last() {
                // Grab the id of the next statement to be executed AFTER the inner_body has finished executing
                let exit_scope_id = next_st.get_id();
                self.scope_manager
                    .push(ScopeState::new(exit_scope_id, self.sigma.clone()))
            }
        }

        // Now add the guard to the path
        self.path.branch(guard);

        // Add the inner scope statements to the stack
        for s in inner_body {
            self.stack.push(s);
        }
        self
    }

    // Going to ignore function/macro calls for now.
    pub fn step(mut self) -> Status {
        let s = self.stack.pop();
        println!("Statement: {:?}", s);
        match s {
            Some(s) => {
                // First need to check whether we need to restore the state
                self.check_scope(&s);
                match s.kind {
                    StatementKind::Skip => todo!(),
                    StatementKind::Assignment(var, e) => {
                        // Apply substitution map to expression to get new value for the variable...
                        let val = e.substitute_and_get_root(&self.sigma);
                        self.sigma.insert(var, val);
                        Status::Continue(self)
                    }
                    StatementKind::Sample(var) => {
                        let sym_name = self.sample();
                        self.sigma.insert(var, ExprNode::new_sample_var(sym_name));
                        Status::Continue(self)
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

                        let states = match (true_sat, false_sat) {
                            (true, true) => vec![
                                self.clone().fork(tru_branch, guard.clone()),
                                self.fork(fls_branch, guard.not()),
                            ],
                            (true, false) => vec![self.fork(tru_branch, guard)],
                            (false, true) => vec![self.fork(fls_branch, guard.not())],
                            (false, false) => Vec::new(),
                        };
                        Status::Fork(states)
                    }
                    // Currently nothing special about how this is done...
                    StatementKind::While(mut guard, body) => {
                        // Apply sigma on the branch guard
                        guard.substitute(&self.sigma);

                        let (true_sat, false_sat) = self.smt_manager.check_fork_satisfiability(
                            self.path.get_conds(),
                            &guard,
                            &self.sym_vars,
                        );

                        let states = match (true_sat, false_sat) {
                            (true, true) => vec![
                                self.clone().fork(body, guard.clone()),
                                self.fork(Vec::new(), guard.not()),
                            ],
                            (true, false) => vec![self.fork(body, guard)],
                            (false, true) => vec![self.fork(Vec::new(), guard.not())],
                            (false, false) => Vec::new(),
                        };
                        Status::Fork(states)
                    }
                    StatementKind::Return(_) => todo!(),
                }
            }
            None => Status::Terminate(self.path),
        }
    }
}
