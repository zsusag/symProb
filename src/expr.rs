use anyhow::{bail, ensure, Context, Result};
use pest::Parser;
use std::{collections::HashMap, fmt::Display};
use z3::{
    ast::{Ast, Bool, Real},
    Context as Ctx,
};

use crate::{
    parser::{parse_expr, ExprParser, Rule},
    path::Sigma,
    semantics::{Gamma, SemanticsError},
    syntax::{ExprKind, Type, Value},
};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Expr {
    root: ExprNode,
}

impl<'ctx> Expr {
    pub fn new(root: ExprNode) -> Self {
        Expr { root }
    }

    pub fn parse(data: &str) -> Result<Self> {
        ExprParser::parse(Rule::expr, data)
            .context("failed to parse expression")
            .map(|token_pairs| Expr::new(parse_expr(token_pairs)))
    }

    pub fn root(self) -> ExprNode {
        self.root
    }
    // Returns the type of the expression
    pub fn typecheck(&self, fn_sigs: &HashMap<&String, Vec<&Type>>, gamma: &Gamma) -> Result<Type> {
        self.root.typecheck(fn_sigs, gamma)
    }

    pub fn substitute_and_get_root(mut self, sigma: &Sigma) -> ExprNode {
        self.root.substitute(sigma);
        self.root
    }

    pub fn substitute(&mut self, sigma: &Sigma) {
        self.root.substitute(sigma);
    }

    pub fn clone_and_substitute(&self, sigma: &Sigma) -> Expr {
        let mut copy = self.clone();
        copy.root.substitute(sigma);
        copy
    }

    pub fn convert(&self, ctx: &'ctx Ctx) -> Bool<'ctx> {
        self.root.convert_bool(ctx)
    }

    pub fn not(self) -> Self {
        Expr::new(ExprNode::new(ExprKind::Not, vec![self.root]))
    }

    pub fn to_psi_expr(&self) -> PsiExpr {
        self.root.to_psi_expr()
    }

    pub fn simplify(&mut self) {
        self.root.simplify();
    }

    /// Wraps the expression in Iverson brackets.
    pub fn iverson(self) -> Expr {
        Expr::new(ExprNode::new(ExprKind::Iverson, vec![self.root]))
    }

    /// Wraps the expression in Iverson brackets by mutating `self`.
    pub fn iverson_mut(&mut self) {
        let new_root = ExprNode::new(ExprKind::Iverson, Vec::with_capacity(1));
        let old_root = std::mem::replace(&mut self.root, new_root);
        self.root.children.push(old_root);
    }

    /// Applies the boolean AND operation between `self` and `expr` by mutating `self`.
    ///
    /// **Warning**: This method assumes that both `self` and `expr` have type `Type::Bool`. If
    /// either `self` or `expr` have type `Type::Real`, then the two expressions will still be
    /// logically AND'd together but the expression will be unsound.
    fn and(&mut self, expr: Expr) {
        let and_node = ExprNode::new(ExprKind::And, Vec::with_capacity(2));
        let c1 = std::mem::replace(&mut self.root, and_node);
        self.root.children.push(c1);
        self.root.children.push(expr.root);
    }

    /// Returns the expression `self ∧ expr`.
    ///
    /// **Warning**: This method assumes that both `self` and `expr` have type `Type::Bool`. If
    /// either `self` or `expr` have type `Type::Real`, then the two expressions will still be
    /// logically AND'd together but the expression will be unsound.
    fn into_and(self, expr: Expr) -> Expr {
        Expr::new(ExprNode::new(ExprKind::And, vec![self.root, expr.root]))
    }
}

impl<T> From<T> for Expr
where
    T: Into<ExprNode>,
{
    fn from(value: T) -> Self {
        Expr::new(value.into())
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct ExprNode {
    e: ExprKind,
    children: Vec<ExprNode>,
}

impl<'ctx> ExprNode {
    pub fn new(e: ExprKind, children: Vec<ExprNode>) -> Self {
        ExprNode { e, children }
    }

    pub fn new_leaf(e: ExprKind) -> Self {
        ExprNode {
            e,
            children: Vec::new(),
        }
    }

    pub fn new_sample_var(name: String) -> Self {
        ExprNode {
            e: ExprKind::Constant(Value::Var(name)),
            children: Vec::new(),
        }
    }

    pub fn get_mut_e(&mut self) -> &mut ExprKind {
        &mut self.e
    }

    pub fn get_e(&self) -> &ExprKind {
        &self.e
    }

    pub fn needs_parens(&self) -> bool {
        self.children.len() > 1
    }

    fn is_constant(&self) -> bool {
        match &self.e {
            ExprKind::Constant(val) => match val {
                Value::Num(_) | Value::Boolean(_) => true,
                Value::Var(_) => false,
            },
            _ => false,
        }
    }

    pub fn simplify(&mut self) {
        let mut can_reduce = false;

        for child in self.children.iter_mut() {
            child.simplify();
            can_reduce |= child.is_constant();
        }
        if can_reduce & !self.children.is_empty() {
            match &self.e {
                ExprKind::Constant(_) | ExprKind::Sqrt | ExprKind::Iverson => (),
                ExprKind::Add => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    match (&c1.e, &c2.e) {
                        (
                            ExprKind::Constant(Value::Num(x1)),
                            ExprKind::Constant(Value::Num(x2)),
                        ) => {
                            self.e = ExprKind::Constant(Value::Num(x1 + x2));
                        }
                        (ExprKind::Add, ExprKind::Constant(Value::Num(x2))) => {
                            let c1a = c1.children.get(0).unwrap();
                            let c1b = c1.children.get(1).unwrap();
                            if let ExprKind::Constant(Value::Num(x1)) = c1b.e {
                                self.children.push(c1a.clone());
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 + x2),
                                )));
                            } else if let ExprKind::Constant(Value::Num(x1)) = c1a.e {
                                self.children.push(c1b.clone());
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 + x2),
                                )));
                            }
                        }
                        (ExprKind::Constant(Value::Num(x1)), ExprKind::Add) => {
                            let c2a = c2.children.get(0).unwrap();
                            let c2b = c2.children.get(1).unwrap();
                            if let ExprKind::Constant(Value::Num(x2)) = c2b.e {
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 + x2),
                                )));
                                self.children.push(c2a.clone());
                            } else if let ExprKind::Constant(Value::Num(x2)) = c2a.e {
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 + x2),
                                )));
                                self.children.push(c2b.clone());
                            }
                        }
                        (ExprKind::Sub, ExprKind::Constant(Value::Num(x2))) => {
                            let c1a = c1.children.get(0).unwrap();
                            let c1b = c1.children.get(1).unwrap();
                            if let ExprKind::Constant(Value::Num(x1)) = c1b.e {
                                self.children.push(c1a.clone());
                                if x1 - x2 > 0.into() {
                                    self.e = ExprKind::Sub;
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x1 - x2),
                                    )));
                                } else {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x2 - x1),
                                    )));
                                }
                            } else if let ExprKind::Constant(Value::Num(x1)) = c1a.e {
                                self.e = ExprKind::Sub;
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 + x2),
                                )));
                                self.children.push(c1b.clone());
                            }
                        }
                        (ExprKind::Constant(Value::Num(x1)), ExprKind::Sub) => {
                            let c2a = c2.children.get(0).unwrap();
                            let c2b = c2.children.get(1).unwrap();
                            if let ExprKind::Constant(Value::Num(x2)) = c2b.e {
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 - x2),
                                )));
                                self.children.push(c2a.clone());
                            } else if let ExprKind::Constant(Value::Num(x2)) = c2a.e {
                                self.e = ExprKind::Sub;
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 + x2),
                                )));
                                self.children.push(c2b.clone());
                            }
                        }

                        _ => {
                            self.children.push(c1);
                            self.children.push(c2);
                        }
                    }
                }
                ExprKind::Sub => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    match (&c1.e, &c2.e) {
                        (
                            ExprKind::Constant(Value::Num(x1)),
                            ExprKind::Constant(Value::Num(x2)),
                        ) => {
                            self.e = ExprKind::Constant(Value::Num(x1 - x2));
                        }
                        (ExprKind::Add, ExprKind::Constant(Value::Num(x2))) => {
                            let c1a = c1.children.get(0).unwrap();
                            let c1b = c1.children.get(1).unwrap();
                            if let ExprKind::Constant(Value::Num(x1)) = c1b.e {
                                self.children.push(c1a.clone());
                                if x1 - x2 >= 0.into() {
                                    self.e = ExprKind::Add;
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x1 - x2),
                                    )));
                                } else {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x2 - x1),
                                    )));
                                }
                            } else if let ExprKind::Constant(Value::Num(x1)) = c1a.e {
                                self.children.push(c1b.clone());
                                if x1 - x2 >= 0.into() {
                                    self.e = ExprKind::Add;
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x1 - x2),
                                    )));
                                } else {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x2 - x1),
                                    )));
                                }
                            }
                        }
                        (ExprKind::Constant(Value::Num(x1)), ExprKind::Add) => {
                            let c2a = c2.children.get(0).unwrap();
                            let c2b = c2.children.get(1).unwrap();
                            if let ExprKind::Constant(Value::Num(x2)) = c2b.e {
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 - x2),
                                )));
                                self.children.push(c2a.clone());
                            } else if let ExprKind::Constant(Value::Num(x2)) = c2a.e {
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 - x2),
                                )));
                                self.children.push(c2b.clone());
                            }
                        }
                        (ExprKind::Sub, ExprKind::Constant(Value::Num(x2))) => {
                            let c1a = c1.children.get(0).unwrap();
                            let c1b = c1.children.get(1).unwrap();
                            if let ExprKind::Constant(Value::Num(x1)) = c1b.e {
                                self.children.push(c1a.clone());
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 + x2),
                                )));
                            } else if let ExprKind::Constant(Value::Num(x1)) = c1a.e {
                                self.children.push(ExprNode::new(
                                    ExprKind::Mul,
                                    [
                                        ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                                            num::rational::Ratio::<i32>::from(-1),
                                        ))),
                                        c1b.clone(),
                                    ]
                                    .to_vec(),
                                ));
                                if x1 - x2 >= 0.into() {
                                    self.e = ExprKind::Add;
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x1 - x2),
                                    )));
                                } else {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x2 - x1),
                                    )));
                                }
                                self.simplify();
                            }
                        }
                        (ExprKind::Constant(Value::Num(x1)), ExprKind::Sub) => {
                            let c2a = c2.children.get(0).unwrap();
                            let c2b = c2.children.get(1).unwrap();
                            if let ExprKind::Constant(Value::Num(x2)) = c2b.e {
                                self.children.push(ExprNode::new(
                                    ExprKind::Mul,
                                    [
                                        ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                                            num::rational::Ratio::<i32>::from(-1),
                                        ))),
                                        c2a.clone(),
                                    ]
                                    .to_vec(),
                                ));
                                self.e = ExprKind::Add;
                                self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                    Value::Num(x1 + x2),
                                )));
                                self.simplify();
                            } else if let ExprKind::Constant(Value::Num(x2)) = c2a.e {
                                self.children.push(c2b.clone());
                                if x1 - x2 >= 0.into() {
                                    self.e = ExprKind::Add;
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x1 - x2),
                                    )));
                                } else {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(
                                        Value::Num(x2 - x1),
                                    )));
                                }
                            }
                        }
                        _ => {
                            self.children.push(c1);
                            self.children.push(c2);
                        }
                    }
                }
                ExprKind::Mul => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    match (&c1.e, &c2.e) {
                        (
                            ExprKind::Constant(Value::Num(x1)),
                            ExprKind::Constant(Value::Num(x2)),
                        ) => {
                            self.e = ExprKind::Constant(Value::Num(x1 * x2));
                        }
                        (ExprKind::Add, ExprKind::Constant(Value::Num(x2))) => {
                            let mut a = ExprNode::new(
                                ExprKind::Mul,
                                [
                                    c1.children.get(0).unwrap().clone(),
                                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x2))),
                                ]
                                .to_vec(),
                            );
                            a.simplify();
                            self.children.push(a);
                            self.e = ExprKind::Add;
                            let mut b = ExprNode::new(
                                ExprKind::Mul,
                                [
                                    c1.children.get(1).unwrap().clone(),
                                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x2))),
                                ]
                                .to_vec(),
                            );
                            b.simplify();
                            self.children.push(b);
                        }
                        (ExprKind::Constant(Value::Num(x1)), ExprKind::Add) => {
                            let mut a = ExprNode::new(
                                ExprKind::Mul,
                                [
                                    c2.children.get(0).unwrap().clone(),
                                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x1))),
                                ]
                                .to_vec(),
                            );
                            a.simplify();
                            self.children.push(a);
                            self.e = ExprKind::Add;
                            let mut b = ExprNode::new(
                                ExprKind::Mul,
                                [
                                    c2.children.get(1).unwrap().clone(),
                                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x1))),
                                ]
                                .to_vec(),
                            );
                            b.simplify();
                            self.children.push(b);
                        }
                        (ExprKind::Sub, ExprKind::Constant(Value::Num(x2))) => {
                            let mut a = ExprNode::new(
                                ExprKind::Mul,
                                [
                                    c1.children.get(0).unwrap().clone(),
                                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x2))),
                                ]
                                .to_vec(),
                            );
                            a.simplify();
                            self.children.push(a);
                            self.e = ExprKind::Sub;
                            let mut b = ExprNode::new(
                                ExprKind::Mul,
                                [
                                    c1.children.get(1).unwrap().clone(),
                                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x2))),
                                ]
                                .to_vec(),
                            );
                            b.simplify();
                            self.children.push(b);
                        }
                        (ExprKind::Constant(Value::Num(x1)), ExprKind::Sub) => {
                            let mut a = ExprNode::new(
                                ExprKind::Mul,
                                [
                                    c2.children.get(0).unwrap().clone(),
                                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x1))),
                                ]
                                .to_vec(),
                            );
                            a.simplify();
                            self.children.push(a);
                            self.e = ExprKind::Sub;
                            let mut b = ExprNode::new(
                                ExprKind::Mul,
                                [
                                    c2.children.get(1).unwrap().clone(),
                                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x1))),
                                ]
                                .to_vec(),
                            );
                            b.simplify();
                            self.children.push(b);
                        }
                        _ => {
                            self.children.push(c1);
                            self.children.push(c2);
                        }
                    }
                }
                ExprKind::Div => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Num(x1)),
                        ExprKind::Constant(Value::Num(x2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Num(x1 / x2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::And => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Boolean(b1)),
                        ExprKind::Constant(Value::Boolean(b2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Boolean(*b1 && *b2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::Or => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Boolean(b1)),
                        ExprKind::Constant(Value::Boolean(b2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Boolean(*b1 || *b2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::Not => {
                    if let ExprKind::Constant(Value::Boolean(b)) = self.children.pop().unwrap().e {
                        self.e = ExprKind::Constant(Value::Boolean(!b));
                    }
                }
                ExprKind::Lt => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Num(x1)),
                        ExprKind::Constant(Value::Num(x2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Boolean(x1 < x2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::Le => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Num(x1)),
                        ExprKind::Constant(Value::Num(x2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Boolean(x1 <= x2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::Gt => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Num(x1)),
                        ExprKind::Constant(Value::Num(x2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Boolean(x1 > x2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::Ge => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Num(x1)),
                        ExprKind::Constant(Value::Num(x2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Boolean(x1 >= x2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::Eq => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Num(x1)),
                        ExprKind::Constant(Value::Num(x2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Boolean(x1 == x2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::Ne => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    if let (
                        ExprKind::Constant(Value::Num(x1)),
                        ExprKind::Constant(Value::Num(x2)),
                    ) = (&c1.e, &c2.e)
                    {
                        self.e = ExprKind::Constant(Value::Boolean(x1 != x2));
                    } else {
                        self.children.push(c1);
                        self.children.push(c2);
                    }
                }
                ExprKind::Func(_) => unreachable!(),
            };
        }
    }

    fn typecheck(&self, fn_sigs: &HashMap<&String, Vec<&Type>>, gamma: &Gamma) -> Result<Type> {
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
            ExprKind::Sqrt => {
                let c = self
                    .children
                    .first()
                    .ok_or_else(|| SemanticsError::PartialFn {
                        fn_name: "sqrt".to_string(),
                        num_args_expected: 1,
                        num_args_given: 0,
                    })?;
                let c_t = c.typecheck(fn_sigs, gamma)?;
                ensure!(
                    c_t == Type::Real,
                    SemanticsError::TypeError {
                        expected: Type::Real,
                        found: c_t,
                        e: Expr::new(self.to_owned())
                    }
                );
                Ok(Type::Real)
            }
            ExprKind::Iverson => {
                let c = self
                    .children
                    .first()
                    .ok_or_else(|| SemanticsError::PartialFn {
                        fn_name: "Iverson".to_string(),
                        num_args_expected: 1,
                        num_args_given: 0,
                    })?;
                let c_t = c.typecheck(fn_sigs, gamma)?;
                ensure!(
                    c_t == Type::Bool,
                    SemanticsError::TypeError {
                        expected: Type::Bool,
                        found: c_t,
                        e: Expr::new(self.to_owned())
                    }
                );
                Ok(Type::Real)
            }
        }
    }

    // WARNING: Assumes there are no function calls
    pub fn substitute(&mut self, sigma: &Sigma) {
        if let ExprKind::Constant(Value::Var(name)) = &self.e {
            if let Some(expr) = sigma.get(name) {
                *self = expr.root.clone();
            }
        } else {
            for child in self.children.iter_mut() {
                child.substitute(sigma);
            }
        }
    }

    pub fn convert_bool(&self, ctx: &'ctx Ctx) -> Bool<'ctx> {
        match &self.e {
            ExprKind::Constant(v) => match v {
                Value::Num(_) => unreachable!(),
                Value::Boolean(b) => Bool::from_bool(ctx, *b),
                Value::Var(name) => {
                    // Assuming that we got here... declare name as a boolean variable
                    Bool::new_const(ctx, name.as_str())
                }
            },
            ExprKind::And => {
                let children: Vec<_> = self.children.iter().map(|c| c.convert_bool(ctx)).collect();
                let refs: Vec<_> = children.iter().collect();
                Bool::and(ctx, refs.as_slice())
            }
            ExprKind::Or => {
                let children: Vec<_> = self.children.iter().map(|c| c.convert_bool(ctx)).collect();
                let refs: Vec<_> = children.iter().collect();
                Bool::or(ctx, refs.as_slice())
            }
            ExprKind::Not => self.children.first().unwrap().convert_bool(ctx).not(),
            ExprKind::Lt => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1.lt(&e2)
            }
            ExprKind::Le => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1.le(&e2)
            }
            ExprKind::Gt => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1.gt(&e2)
            }
            ExprKind::Ge => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1.ge(&e2)
            }
            ExprKind::Eq => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1._eq(&e2)
            }
            ExprKind::Ne => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                !e1._eq(&e2)
            }
            _ => unreachable!(),
        }
    }

    pub fn convert_real(&self, ctx: &'ctx Ctx) -> Real<'ctx> {
        match &self.e {
            ExprKind::Constant(v) => match v {
                Value::Num(n) => Real::from_real(ctx, *n.numer(), *n.denom()),
                Value::Boolean(_) => unreachable!(),
                Value::Var(name) => Real::new_const(ctx, name.as_str()),
            },
            ExprKind::Add => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1 + e2
            }
            ExprKind::Sub => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1 - e2
            }
            ExprKind::Mul => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1 * e2
            }
            ExprKind::Div => {
                let e1 = self.children.get(0).unwrap().convert_real(ctx);
                let e2 = self.children.get(1).unwrap().convert_real(ctx);
                e1 / e2
            }
            ExprKind::Sqrt => {
                let e = self.children.get(0).unwrap().convert_real(ctx);
                let exponent = Real::from_real(ctx, 1, 2);
                e.power(&exponent)
            }
            _ => unreachable!(),
        }
    }

    pub fn to_psi_expr(&self) -> PsiExpr {
        PsiExpr(self.clone())
    }
}

impl<T> From<T> for ExprNode
where
    T: Into<ExprKind>,
{
    fn from(value: T) -> Self {
        ExprNode::new_leaf(value.into())
    }
}

pub struct PsiExpr(ExprNode);

impl Display for PsiExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0.e {
            ExprKind::Constant(v) => match v {
                Value::Num(n) => {
                    if *n.denom() == 1 {
                        write!(f, "{}", n.numer())
                    } else {
                        write!(f, "({}/{})", n.numer(), n.denom())
                    }
                }
                Value::Boolean(b) => write!(f, "{}", b),
                Value::Var(name) => write!(f, "{}", name),
            },
            ExprKind::Add => write!(
                f,
                "({}) + ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Sub => write!(
                f,
                "({}) - ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Mul => write!(
                f,
                "({}) * ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Div => write!(
                f,
                "({}) / ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Sqrt => write!(
                f,
                "({}) ^ (1/2)",
                self.0.children.get(0).unwrap().to_psi_expr()
            ),
            ExprKind::And => write!(
                f,
                "({}) && ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Or => write!(
                f,
                "({}) || ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Not => write!(f, "!({})", self.0.children.get(0).unwrap().to_psi_expr()),
            ExprKind::Lt => write!(
                f,
                "({}) < ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Le => write!(
                f,
                "({}) <= ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Gt => write!(
                f,
                "({}) > ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Ge => write!(
                f,
                "({}) >= ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Eq => write!(
                f,
                "({}) == ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Ne => write!(
                f,
                "({}) != ({})",
                self.0.children.get(0).unwrap().to_psi_expr(),
                self.0.children.get(1).unwrap().to_psi_expr()
            ),
            ExprKind::Func(_) => todo!(),
            ExprKind::Iverson => write!(f, "[{}]", self.0.children.get(0).unwrap().to_psi_expr()),
        }
    }
}

// Pretty printing!
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.root)
    }
}

impl Display for ExprNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.e {
            ExprKind::Constant(_) => write!(f, "{}", self.e),
            ExprKind::Add
            | ExprKind::Sub
            | ExprKind::Mul
            | ExprKind::And
            | ExprKind::Or
            | ExprKind::Div => write!(
                f,
                "{} {} {}",
                self.children.get(0).unwrap(),
                self.e,
                self.children.get(1).unwrap()
            ),
            ExprKind::Sqrt => write!(f, "{}{}", self.e, self.children.get(0).unwrap()),
            ExprKind::Lt
            | ExprKind::Le
            | ExprKind::Gt
            | ExprKind::Ge
            | ExprKind::Eq
            | ExprKind::Ne => {
                let e1 = self.children.get(0).unwrap();
                let e2 = self.children.get(1).unwrap();
                match (e1.needs_parens(), e2.needs_parens()) {
                    (true, true) => write!(f, "({}) {} ({})", e1, self.e, e2),
                    (true, false) => write!(f, "({}) {} {}", e1, self.e, e2),
                    (false, true) => write!(f, "{} {} ({})", e1, self.e, e2),
                    (false, false) => write!(f, "{} {} {}", e1, self.e, e2),
                }
            }
            ExprKind::Not => {
                let e = self.children.get(0).unwrap();
                if e.needs_parens() {
                    write!(f, "{}({})", self.e, e)
                } else {
                    write!(f, "{}{}", self.e, e)
                }
            }
            ExprKind::Func(_) => todo!(),
            ExprKind::Iverson => write!(f, "[{}]", self.children.get(0).unwrap()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PostExpectation(Expr);

impl PostExpectation {
    /// Parse a postexpectation from a string as an expression.
    ///
    /// The `PostExpectation` returned (if parsing was successful) is untyped.
    pub fn parse(data: &str) -> Result<Self> {
        Expr::parse(data).map(PostExpectation)
    }

    /// Checks whether the postexpectation is real-valued given a substitution [`Sigma`]. If the
    /// postexpectation is boolean-valued then the postexpectation is wrapped in Iverson brackets
    /// thereby converting it into a real-valued expression.
    ///
    /// This method returns an `Err` if the postexpectation is ill-typed or if there are variables
    /// in the postexpectation which are not in `sigma`.
    pub fn typecheck(&mut self, sigma: &Sigma) -> Result<()> {
        // Function calls within postexpectations are not supported (yet).
        let fn_sigs = HashMap::new();
        let gamma = Gamma::from_sigma(sigma)?;
        let post_type = self.0.typecheck(&fn_sigs, &gamma)?;

        // Wrap the postexpectation in Iverson brackets, if necessary.
        if let Type::Bool = post_type {
            self.0.iverson_mut();
        }
        Ok(())
    }

    /// Applies the substitution to the postexpectation.
    pub fn apply_sigma(&mut self, sigma: &Sigma) {
        sigma.apply(&mut self.0)
    }
}
