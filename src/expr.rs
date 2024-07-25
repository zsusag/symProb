use anyhow::{bail, ensure, Result};
use std::{collections::HashMap, fmt::Display};
use z3::{
    ast::{Ast, Bool, Real},
    Context,
};

use crate::{
    semantics::SemanticsError,
    syntax::{ExprKind, Type, Value},
};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Expr {
    root: ExprNode,
    //vars: BTreeSet<String>, // Unsure if I need...
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct ExprNode {
    e: ExprKind,
    children: Vec<ExprNode>,
}

impl<'ctx> Expr {
    pub fn new(root: ExprNode) -> Self {
        Expr { root }
    }

    pub fn root(self) -> ExprNode {
        self.root
    }
    // Returns the type of the expression
    pub fn typecheck(
        &self,
        fn_sigs: &HashMap<&String, Vec<&Type>>,
        gamma: &HashMap<&String, Type>,
    ) -> Result<Type> {
        self.root.typecheck(fn_sigs, gamma)
    }

    pub fn substitute_and_get_root(mut self, sigma: &HashMap<String, ExprNode>) -> ExprNode {
        self.root.substitute(sigma);
        self.root
    }

    pub fn substitute(&mut self, sigma: &HashMap<String, ExprNode>) {
        self.root.substitute(sigma);
    }

    pub fn clone_and_substitute(&self, sigma: &HashMap<String, ExprNode>) -> Expr {
        let mut copy = self.clone();
        copy.root.substitute(sigma);
        copy
    }

    pub fn convert(&self, ctx: &'ctx Context) -> Bool<'ctx> {
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
                ExprKind::Constant(_) | ExprKind::Sqrt => (),
                ExprKind::Add => {
                    let c2 = self.children.pop().unwrap();
                    let c1 = self.children.pop().unwrap();
                    match (&c1.e, &c2.e) {
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Constant(Value::Num(x2)))
                            => { self.e = ExprKind::Constant(Value::Num(x1 + x2)); }
                        (ExprKind::Add,ExprKind::Constant(Value::Num(x2)))
                            => {
                                let c1a = c1.children.get(0).unwrap();
                                let c1b = c1.children.get(1).unwrap();
                                if let ExprKind::Constant(Value::Num(x1)) = c1b.e {
                                    self.children.push(c1a.clone());
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1+x2))));
                                } else if let ExprKind::Constant(Value::Num(x1)) = c1a.e {
                                    self.children.push(c1b.clone());
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1+x2))));
                                }
                            }
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Add)
                            => {
                                let c2a = c2.children.get(0).unwrap();
                                let c2b = c2.children.get(1).unwrap();
                                if let ExprKind::Constant(Value::Num(x2)) = c2b.e {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1+x2))));
                                    self.children.push(c2a.clone());
                                } else if let ExprKind::Constant(Value::Num(x2)) = c2a.e {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1+x2))));
                                    self.children.push(c2b.clone());
                                } 
                            }
                        (ExprKind::Sub,ExprKind::Constant(Value::Num(x2)))
                            => {
                                let c1a = c1.children.get(0).unwrap();
                                let c1b = c1.children.get(1).unwrap();
                                if let ExprKind::Constant(Value::Num(x1)) = c1b.e {
                                    self.children.push(c1a.clone());
                                    if x1-x2 > 0.into() {
                                        self.e = ExprKind::Sub;
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1-x2))));
                                    } else {
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x2-x1))));
                                    }
                                } else if let ExprKind::Constant(Value::Num(x1)) = c1a.e {
                                    self.e = ExprKind::Sub;
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1+x2))));
                                    self.children.push(c1b.clone());
                                }
                            }
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Sub)
                            => {
                                let c2a = c2.children.get(0).unwrap();
                                let c2b = c2.children.get(1).unwrap();
                                if let ExprKind::Constant(Value::Num(x2)) = c2b.e {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1-x2))));
                                    self.children.push(c2a.clone());
                                } else if let ExprKind::Constant(Value::Num(x2)) = c2a.e {
                                    self.e = ExprKind::Sub;
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1+x2))));
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
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Constant(Value::Num(x2)))
                            => { self.e = ExprKind::Constant(Value::Num(x1 - x2)); }
                        (ExprKind::Add,ExprKind::Constant(Value::Num(x2)))
                            => {
                                let c1a = c1.children.get(0).unwrap();
                                let c1b = c1.children.get(1).unwrap();
                                if let ExprKind::Constant(Value::Num(x1)) = c1b.e {
                                    self.children.push(c1a.clone());
                                    if x1-x2 >= 0.into() {
                                        self.e = ExprKind::Add;
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1-x2))));
                                    } else {
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x2-x1))));
                                    }
                                } else if let ExprKind::Constant(Value::Num(x1)) = c1a.e {
                                    self.children.push(c1b.clone());
                                    if x1-x2 >= 0.into() {
                                        self.e = ExprKind::Add;
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1-x2))));
                                    } else {
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x2-x1))));
                                    }
                                }
                            }
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Add)
                            => {
                                let c2a = c2.children.get(0).unwrap();
                                let c2b = c2.children.get(1).unwrap();
                                if let ExprKind::Constant(Value::Num(x2)) = c2b.e {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1-x2))));
                                    self.children.push(c2a.clone());
                                } else if let ExprKind::Constant(Value::Num(x2)) = c2a.e {
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1-x2))));
                                    self.children.push(c2b.clone());
                                } 
                            }
                        (ExprKind::Sub,ExprKind::Constant(Value::Num(x2)))
                            => {
                                let c1a = c1.children.get(0).unwrap();
                                let c1b = c1.children.get(1).unwrap();
                                if let ExprKind::Constant(Value::Num(x1)) = c1b.e {
                                    self.children.push(c1a.clone());
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1+x2))));
                                } else if let ExprKind::Constant(Value::Num(x1)) = c1a.e {
                                    self.children.push(ExprNode::new(ExprKind::Mul,[ExprNode::new_leaf(ExprKind::Constant(Value::Num(num::rational::Ratio::<i32>::from(-1)))),c1b.clone()].to_vec()));
                                    if x1-x2 >= 0.into() {
                                        self.e = ExprKind::Add;
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1-x2))));
                                    } else {
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x2-x1))));
                                    }
                                    self.simplify();
                                }
                            }
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Sub)
                            => {
                                let c2a = c2.children.get(0).unwrap();
                                let c2b = c2.children.get(1).unwrap();
                                if let ExprKind::Constant(Value::Num(x2)) = c2b.e {
                                    self.children.push(ExprNode::new(ExprKind::Mul,[ExprNode::new_leaf(ExprKind::Constant(Value::Num(num::rational::Ratio::<i32>::from(-1)))),c2a.clone()].to_vec()));
                                    self.e=ExprKind::Add;
                                    self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1+x2))));
                                    self.simplify();
                                } else if let ExprKind::Constant(Value::Num(x2)) = c2a.e {
                                    self.children.push(c2b.clone());
                                    if x1-x2 >= 0.into() {
                                        self.e = ExprKind::Add;
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x1-x2))));
                                    } else {
                                        self.children.push(ExprNode::new_leaf(ExprKind::Constant(Value::Num(x2-x1))));
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
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Constant(Value::Num(x2)))
                            => { self.e = ExprKind::Constant(Value::Num(x1 * x2)); }
                        (ExprKind::Add,ExprKind::Constant(Value::Num(x2)))
                            => {
                                let mut a = ExprNode::new(ExprKind::Mul,[c1.children.get(0).unwrap().clone(),ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x2)))].to_vec());
                                a.simplify();
                                self.children.push(a);
                                self.e = ExprKind::Add;
                                let mut b = ExprNode::new(ExprKind::Mul,[c1.children.get(1).unwrap().clone(),ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x2)))].to_vec());
                                b.simplify();
                                self.children.push(b);
                            }
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Add)
                            => {
                                let mut a = ExprNode::new(ExprKind::Mul,[c2.children.get(0).unwrap().clone(),ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x1)))].to_vec());
                                a.simplify();
                                self.children.push(a);
                                self.e = ExprKind::Add;
                                let mut b = ExprNode::new(ExprKind::Mul,[c2.children.get(1).unwrap().clone(),ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x1)))].to_vec());
                                b.simplify();
                                self.children.push(b);
                            }
                        (ExprKind::Sub,ExprKind::Constant(Value::Num(x2)))
                            => {
                                let mut a = ExprNode::new(ExprKind::Mul,[c1.children.get(0).unwrap().clone(),ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x2)))].to_vec());
                                a.simplify();
                                self.children.push(a);
                                self.e = ExprKind::Sub;
                                let mut b = ExprNode::new(ExprKind::Mul,[c1.children.get(1).unwrap().clone(),ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x2)))].to_vec());
                                b.simplify();
                                self.children.push(b);
                            }
                        (ExprKind::Constant(Value::Num(x1)),ExprKind::Sub)
                            => {
                                let mut a = ExprNode::new(ExprKind::Mul,[c2.children.get(0).unwrap().clone(),ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x1)))].to_vec());
                                a.simplify();
                                self.children.push(a);
                                self.e = ExprKind::Sub;
                                let mut b = ExprNode::new(ExprKind::Mul,[c2.children.get(1).unwrap().clone(),ExprNode::new_leaf(ExprKind::Constant(Value::Num(*x1)))].to_vec());
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
                    } else {
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
            ExprKind::Sqrt => unreachable!(),
        }
    }

    // WARNING: Assumes there are no function calls
    pub fn substitute(&mut self, sigma: &HashMap<String, ExprNode>) {
        if let ExprKind::Constant(Value::Var(name)) = &self.e {
            *self = sigma.get(name).unwrap().clone();
        } else {
            for child in self.children.iter_mut() {
                child.substitute(sigma);
            }
        }
    }

    pub fn convert_bool(&self, ctx: &'ctx Context) -> Bool<'ctx> {
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

    pub fn convert_real(&self, ctx: &'ctx Context) -> Real<'ctx> {
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
        }
    }
}
