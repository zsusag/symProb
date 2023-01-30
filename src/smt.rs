use std::collections::HashMap;

use z3::{
    ast::{Bool, Real},
    Config, Context, SatResult, Solver,
};

use crate::{executor_state::SymType, expr::Expr};

#[derive(Debug)]
pub struct SMTMangager {
    cfg: Config,
    pub ctx: Context,
}

impl Clone for SMTMangager {
    fn clone(&self) -> Self {
        SMTMangager::new()
    }
}

fn sat_to_bool(res: &SatResult) -> bool {
    match res {
        SatResult::Unsat => false,
        SatResult::Unknown => panic!("Z3 returned unknown"),
        SatResult::Sat => true,
    }
}

impl<'ctx> SMTMangager {
    pub fn new() -> Self {
        let cfg = Config::new();
        let ctx = Context::new(&cfg);
        SMTMangager { cfg, ctx }
    }

    // Returns (b_true, b_false) where b_true is true if the true branch is satisfiable, same for false branch
    pub fn check_fork_satisfiability(
        &'ctx self,
        cur_path: &[Expr],
        guard: &Expr,
        sym_vars: &HashMap<String, SymType>,
    ) -> (bool, bool) {
        let s = self.init(cur_path, sym_vars);

        let true_guard = guard.convert(&self.ctx);
        let false_guard = true_guard.not();

        let true_res = s.check_assumptions(&[true_guard]);
        let false_res = s.check_assumptions(&[false_guard]);

        (sat_to_bool(&true_res), sat_to_bool(&false_res))
    }

    pub fn is_sat(&'ctx self, formula: &[Expr], sym_vars: &HashMap<String, SymType>) -> bool {
        let s = self.init(formula, sym_vars);
        sat_to_bool(&s.check())
    }

    fn init(&'ctx self, formula: &[Expr], sym_vars: &HashMap<String, SymType>) -> Solver {
        // Make new solver
        let s = Solver::new(&self.ctx);

        for cond in formula.iter() {
            s.assert(&cond.convert(&self.ctx));
        }

        // Add assertions for the bounds of all probabilistic symbolic variables
        sym_vars
            .iter()
            .filter_map(|(name, st)| match st {
                SymType::Normal(_) => None,
                SymType::UniformProb => {
                    let var = Real::new_const(&self.ctx, name.as_str());
                    let zero = Real::from_real(&self.ctx, 0, 1);
                    let one = Real::from_real(&self.ctx, 1, 1);
                    let lower = var.ge(&zero);
                    let upper = var.le(&one);
                    Some(Bool::and(&self.ctx, &[&lower, &upper]))
                }
                SymType::NormalProb => {
                    let var = Real::new_const(&self.ctx, name.as_str());
                    let neg_one = Real::from_real(&self.ctx, -1, 1);
                    let one = Real::from_real(&self.ctx, 1, 1);
                    let lower = var.ge(&neg_one);
                    let upper = var.le(&one);
                    Some(Bool::and(&self.ctx, &[&lower, &upper]))
                }
            })
            .for_each(|bound| s.assert(&bound));

        s
    }
}
