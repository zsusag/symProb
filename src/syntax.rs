use std::{collections::HashMap, fmt::Display};

use anyhow::Result;
use num::Rational32;

use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Type {
    Real,
    Bool,
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Value {
    Num(Rational32),
    Boolean(bool),
    Var(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", *n),
            Value::Boolean(b) => write!(f, "{}", *b),
            Value::Var(x) => write!(f, "{}", *x),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum ExprKind {
    Constant(Value),
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Not,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Iverson,
    Func(String),
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Constant(v) => write!(f, "{}", *v),
            ExprKind::Add => write!(f, "+"),
            ExprKind::Sub => write!(f, "-"),
            ExprKind::Mul => write!(f, "*"),
            ExprKind::Div => write!(f, "/"),
            ExprKind::And => write!(f, "∧"),
            ExprKind::Or => write!(f, "∨"),
            ExprKind::Not => write!(f, "¬"),
            ExprKind::Lt => write!(f, "<"),
            ExprKind::Le => write!(f, "≤"),
            ExprKind::Gt => write!(f, ">"),
            ExprKind::Ge => write!(f, "≥"),
            ExprKind::Eq => write!(f, "="),
            ExprKind::Ne => write!(f, "≠"),
            ExprKind::Iverson => todo!(),
            ExprKind::Func(_) => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Statement {
    id: u32,
    pub kind: StatementKind,
}

impl Statement {
    pub fn new(kind: StatementKind, counter: &mut u32) -> Self {
        let ret = Statement { id: *counter, kind };
        *counter += 1;
        ret
    }

    pub fn get_id(&self) -> u32 {
        self.id
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum StatementKind {
    Skip,
    Assignment(String, Expr),
    Sample(String),
    Branch(Expr, Vec<Statement>, Vec<Statement>),
    While(Expr, Vec<Statement>),
    Return(Expr),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Func {
    name: String,
    pub inputs: Vec<(String, Type)>,
    pub body: Vec<Statement>,
    ret_t: Option<Type>,
}

impl Func {
    pub fn new(
        name: String,
        inputs: Vec<(String, Type)>,
        body: Vec<Statement>,
        ret_t: Option<Type>,
    ) -> Self {
        Func {
            name,
            inputs,
            body,
            ret_t,
        }
    }

    pub fn construct_gamma(&self) -> HashMap<&String, Type> {
        self.inputs.iter().map(|(x, t)| (x, t.clone())).collect()
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_ret_t(&self) -> &Option<Type> {
        &self.ret_t
    }

    // Return type is always the last type in the returned vector
    pub fn get_type_sig(&self) -> Vec<&Type> {
        let mut sig: Vec<&Type> = self.inputs.iter().map(|(_, t)| t).collect();
        if self.ret_t.is_some() {
            sig.push(self.ret_t.as_ref().unwrap());
        }
        sig
    }

    pub fn get_body(&self) -> &Vec<Statement> {
        &self.body
    }
}
