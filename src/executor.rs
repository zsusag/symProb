use std::collections::{HashMap, HashSet};

use crate::{
    executor_state::{ExecutorState, Status},
    expr::Expr,
    path::Path,
    syntax::Func,
};

pub struct Executor {
    stack: Vec<ExecutorState>,

    paths: HashSet<Path>,

    fn_defs: HashMap<String, Func>,
}

impl Executor {
    // Create an initial state consisting of the statements from the main function
    pub fn new(mut fn_defs: HashMap<String, Func>) -> Self {
        let init_state = ExecutorState::new(fn_defs.remove("main").unwrap());
        let paths = HashSet::new();
        Executor {
            stack: vec![init_state],
            paths,
            fn_defs,
        }
    }

    pub fn run(mut self) -> HashSet<Path> {
        while !self.stack.is_empty() {
            let s = self.stack.pop().unwrap();
            match s.step() {
                Status::Fork(new_states) => {
                    if new_states.is_empty() {
                        // We tried to fork states but unfortunately, both directions were unsatisfiable
                        panic!()
                    } else {
                        for s in new_states {
                            self.stack.push(s);
                        }
                    }
                }
                Status::Continue(s) => self.stack.push(s),
                Status::Terminate(path) => {
                    self.paths.insert(path);
                }
                Status::Return(_) => todo!(),
            }
        }
        self.paths
    }
}
