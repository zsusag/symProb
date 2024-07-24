use std::collections::{HashMap, HashSet};

use anyhow::Result;

use crate::{
    executor_state::{ExecutorState, Status},
    path::Path,
    syntax::{FnMap, Func},
};

pub struct Executor {
    stack: Vec<ExecutorState>,

    paths: HashSet<Path>,
}

impl Executor {
    // Create an initial state consisting of the statements from the main function
    pub fn new(mut fn_defs: FnMap, max_iterations: &Option<u32>) -> Self {
        let init_state = ExecutorState::new(fn_defs.remove("main").unwrap(), max_iterations);
        let paths = HashSet::new();
        Executor {
            stack: vec![init_state],
            paths,
        }
    }

    pub fn run(mut self, prob: bool) -> Result<(HashSet<Path>, usize)> {
        let mut num_failed_observe_paths: usize = 0;
        while !self.stack.is_empty() {
            let s = self.stack.pop().unwrap();
            match s.step(prob)? {
                Status::Fork(true_state, false_state) => {
                    self.stack.push(false_state);
                    self.stack.push(true_state);
                }
                Status::Continue(s) => self.stack.push(s),
                Status::Terminate(path) => {
                    self.paths.insert(path);
                }
                Status::PrematureTerminate => panic!(),
                Status::FailedObserve => {
                    num_failed_observe_paths += 1;
                }
            }
        }
        Ok((self.paths, num_failed_observe_paths))
    }
}
