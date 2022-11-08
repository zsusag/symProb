use std::collections::HashMap;

use anyhow::{bail, ensure, Result};
use thiserror::Error;

use crate::{
    expr::Expr,
    syntax::{Func, Statement, StatementKind, Type},
};

#[derive(Error, Debug)]
pub enum SemanticsError {
    #[error("missing definition of main function")]
    MissingMain,
    #[error("type error (expected {expected:?}, found {found:?} in {e:?})")]
    TypeError {
        expected: Type,
        found: Type,
        e: Expr,
    },
    #[error("missing function return type annotation for \"{fn_name}\"")]
    MissingFnRetType { fn_name: String },
    #[error("undefined variable ({var})")]
    UndefinedVar { var: String },
    #[error("undefined function ({fn_name})")]
    UndefinedFn { fn_name: String },
    #[error("partially applied functions are not supported ({fn_name} expects {num_args_expected}, given {num_args_given})")]
    PartialFn {
        fn_name: String,
        num_args_expected: usize,
        num_args_given: usize,
    },
    #[error("missing path to return function in {fn_name}")]
    MissingRetPath { fn_name: String },
}

pub fn check_valid_program(fns: &HashMap<String, Func>) -> Result<()> {
    // Check to see if there is a main function
    ensure!(fns.contains_key("main"), SemanticsError::MissingMain);

    // Check that all functions (aside from main) has a return type
    for f in fns.values() {
        let name = f.get_name();
        if name != "main" && f.get_ret_t().is_none() {
            bail!(SemanticsError::MissingFnRetType {
                fn_name: name.to_string()
            })
        }
    }

    // Get additional mapping of functions to type signatures
    let fn_sigs: HashMap<&String, Vec<&Type>> = fns
        .iter()
        .map(|(name, f)| (name, f.get_type_sig()))
        .collect();

    // Check for various things in the statements and containing expressions.
    for f in fns.values() {
        // Initially populate gamma with the function parameters
        let mut gamma = f.construct_gamma();
        for s in f.get_body() {
            s.typecheck(&fn_sigs, &mut gamma, f.get_ret_t())?;
        }

        // Ensure that there is a path to a return statement in each function (other than main)
        if f.get_name() != "main" {
            ensure!(
                check_path_to_return(f.get_body()),
                SemanticsError::MissingRetPath {
                    fn_name: f.get_name().to_string(),
                }
            )
        }
    }

    Ok(())
}

fn check_path_to_return(body: &[Statement]) -> bool {
    match &body.last().unwrap().kind {
        StatementKind::Assignment(_, _) | StatementKind::Sample(_) => false,
        StatementKind::Branch(_, true_branch, false_branch) => {
            check_path_to_return(&true_branch) && check_path_to_return(&false_branch)
        }
        StatementKind::While(_, while_body) => check_path_to_return(&while_body),
        StatementKind::Return(_) => true,
    }
}

impl Statement {
    fn typecheck<'a>(
        &'a self,
        fn_sigs: &HashMap<&String, Vec<&Type>>,
        gamma: &mut HashMap<&'a String, Type>,
        ret_t: &Option<Type>,
    ) -> Result<()> {
        match &self.kind {
            StatementKind::Assignment(name, e) => {
                let t = e.typecheck(fn_sigs, gamma)?;
                gamma.insert(&name, t);
            }
            StatementKind::Sample(name) => {
                gamma.insert(&name, Type::Real);
            }
            StatementKind::Branch(cond, true_branch, false_branch) => {
                let t = cond.typecheck(fn_sigs, gamma)?;
                ensure!(
                    t == Type::Bool,
                    SemanticsError::TypeError {
                        expected: Type::Bool,
                        found: Type::Real,
                        e: cond.to_owned(),
                    }
                );
                let mut inner_gamma = gamma.clone();
                for s in true_branch.iter().chain(false_branch.iter()) {
                    s.typecheck(fn_sigs, &mut inner_gamma, ret_t)?;
                }
            }
            StatementKind::While(cond, body) => {
                let t = cond.typecheck(fn_sigs, gamma)?;
                ensure!(
                    t == Type::Bool,
                    SemanticsError::TypeError {
                        expected: Type::Bool,
                        found: Type::Real,
                        e: cond.to_owned(),
                    }
                );
                let mut inner_gamma = gamma.clone();
                for s in body {
                    s.typecheck(fn_sigs, &mut inner_gamma, ret_t)?;
                }
            }
            StatementKind::Return(e) => {
                let actual_ret_t = e.typecheck(fn_sigs, gamma)?;
                let expected_ret_t = ret_t.as_ref().unwrap();
                ensure!(
                    actual_ret_t == *expected_ret_t,
                    SemanticsError::TypeError {
                        expected: expected_ret_t.to_owned(),
                        found: actual_ret_t,
                        e: e.to_owned()
                    }
                );
            }
        };
        Ok(())
    }
}
