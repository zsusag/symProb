use anyhow::Result;

use crate::{
    executor_state::{ExecutorState, Status},
    path::Path,
    symbolic::SymVarMap,
    syntax::FnMap,
};

pub struct Executor {
    stack: Vec<ExecutorState>,
    paths: Vec<Path>,
}

pub struct ExecutorReport {
    pub paths: Vec<Path>,
    pub sym_vars: SymVarMap,
    pub num_failed_observe_paths: usize,
}

impl Executor {
    // Create an initial state consisting of the statements from the main function
    pub fn new(mut fn_defs: FnMap, max_iterations: &Option<u32>) -> Self {
        let init_state = ExecutorState::new(fn_defs.remove("main").unwrap(), max_iterations);
        Executor {
            stack: vec![init_state],
            paths: Vec::new(),
        }
    }

    pub fn run(mut self, prob: bool) -> Result<ExecutorReport> {
        let mut num_failed_observe_paths: usize = 0;
        let mut sym_vars: SymVarMap = SymVarMap::default();
        while let Some(s) = self.stack.pop() {
            match s.step(prob)? {
                Status::Fork(true_state, false_state) => {
                    self.stack.push(false_state);
                    self.stack.push(true_state);
                }
                Status::Continue(s) => self.stack.push(s),
                Status::Terminate(path, path_sym_vars) => {
                    self.paths.push(path);
                    sym_vars.extend(path_sym_vars);
                }
                Status::PrematureTerminate => panic!(),
                Status::FailedObserve => {
                    num_failed_observe_paths += 1;
                }
                Status::MaxIterationsReached(term_path, term_symvars, false_state) => {
                    self.paths.push(term_path);
                    sym_vars.extend(term_symvars);
                    self.stack.push(false_state);
                }
            }
        }
        Ok(ExecutorReport {
            paths: self.paths,
            sym_vars,
            num_failed_observe_paths,
        })
    }
}
