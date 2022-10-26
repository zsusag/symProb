use std::collections::HashMap;

use anyhow::{bail, ensure, Result};

use crate::{
    semantics::SemanticsError,
    syntax::{ExprKind, Type, Value},
};

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Expr {
    root: ExprNode,
    //vars: BTreeSet<String>, // Unsure if I need...
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct ExprNode {
    e: ExprKind,
    children: Vec<ExprNode>,
}

impl Expr {
    pub fn new(root: ExprNode) -> Self {
        Expr { root }
    }
    // Returns the type of the expression
    pub fn typecheck(
        &self,
        fn_sigs: &HashMap<&String, Vec<&Type>>,
        gamma: &HashMap<&String, Type>,
    ) -> Result<Type> {
        self.root.typecheck(fn_sigs, gamma)
    }
}

impl ExprNode {
    pub fn new(e: ExprKind, children: Vec<ExprNode>) -> Self {
        ExprNode { e, children }
    }

    pub fn new_leaf(e: ExprKind) -> Self {
        ExprNode {
            e,
            children: Vec::new(),
        }
    }

    pub fn get_mut_e(&mut self) -> &mut ExprKind {
        &mut self.e
    }

    pub fn get_e(&self) -> &ExprKind {
        &self.e
    }

    fn typecheck(
        &self,
        fn_sigs: &HashMap<&String, Vec<&Type>>,
        gamma: &HashMap<&String, Type>,
    ) -> Result<Type> {
        match &self.e {
            ExprKind::Constant(v) => match v {
                Value::Num(_) => Ok(Type::Real),
                Value::Boolean(_) => Ok(Type::Bool),
                Value::Var(x) => match gamma.get(&x) {
                    Some(t) => Ok(t.clone()),
                    None => bail!(SemanticsError::UndefinedVar { var: x.to_owned() }),
                },
            },
            ExprKind::Add | ExprKind::Sub | ExprKind::Mul | ExprKind::Div => {
                for c in self.children.iter() {
                    let c_t = c.typecheck(fn_sigs, gamma)?;
                    ensure!(
                        Type::Real == c_t,
                        SemanticsError::TypeError {
                            expected: Type::Real,
                            found: c_t,
                            e: Expr::new(self.to_owned())
                        }
                    );
                }
                Ok(Type::Real)
            }
            ExprKind::And | ExprKind::Or | ExprKind::Not => {
                for c in self.children.iter() {
                    let c_t = c.typecheck(fn_sigs, gamma)?;
                    ensure!(
                        Type::Bool == c_t,
                        SemanticsError::TypeError {
                            expected: Type::Bool,
                            found: c_t,
                            e: Expr::new(self.to_owned())
                        }
                    );
                }
                Ok(Type::Bool)
            }
            ExprKind::Lt
            | ExprKind::Le
            | ExprKind::Gt
            | ExprKind::Ge
            | ExprKind::Eq
            | ExprKind::Ne => {
                for c in self.children.iter() {
                    let c_t = c.typecheck(fn_sigs, gamma)?;
                    ensure!(
                        Type::Real == c_t,
                        SemanticsError::TypeError {
                            expected: Type::Real,
                            found: c_t,
                            e: Expr::new(self.to_owned())
                        }
                    );
                }
                Ok(Type::Bool)
            }
            ExprKind::Func(name) => {
                ensure!(name != "main", "cannot call \"main\" function");
                match fn_sigs.get(name) {
                    Some(type_sig) => {
                        ensure!(
                            self.children.len() == (type_sig.len() - 1),
                            SemanticsError::PartialFn {
                                fn_name: name.to_owned(),
                                num_args_expected: type_sig.len() - 1,
                                num_args_given: self.children.len()
                            }
                        );
                        for (e, expected_t) in self.children.iter().zip(type_sig.iter()) {
                            let e_t = e.typecheck(fn_sigs, gamma)?;
                            ensure!(
                                **expected_t == e_t,
                                SemanticsError::TypeError {
                                    expected: (*expected_t).to_owned(),
                                    found: e_t,
                                    e: Expr::new(e.to_owned())
                                }
                            );
                        }
                        Ok((*type_sig.last().unwrap()).to_owned())
                    }
                    None => bail!(SemanticsError::UndefinedFn {
                        fn_name: name.to_owned(),
                    }),
                }
            }
        }
    }
}
