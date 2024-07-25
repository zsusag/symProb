use std::{collections::HashMap, fmt::Display};

use num::Rational32;

use crate::expr::Expr;

#[derive(Debug, PartialEq, Copy, Clone, Eq)]
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

impl From<Rational32> for Value {
    fn from(value: Rational32) -> Self {
        Value::Num(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Var(value)
    }
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
    Sqrt,
    And,
    Or,
    Not,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Func(String),
    Iverson,
}

impl<T> From<T> for ExprKind
where
    T: Into<Value>,
{
    fn from(value: T) -> Self {
        ExprKind::Constant(value.into())
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Constant(v) => write!(f, "{}", *v),
            ExprKind::Add => write!(f, "+"),
            ExprKind::Sub => write!(f, "-"),
            ExprKind::Mul => write!(f, "*"),
            ExprKind::Div => write!(f, "/"),
            ExprKind::Sqrt => write!(f, "√"),
            ExprKind::And => write!(f, "∧"),
            ExprKind::Or => write!(f, "∨"),
            ExprKind::Not => write!(f, "¬"),
            ExprKind::Lt => write!(f, "<"),
            ExprKind::Le => write!(f, "≤"),
            ExprKind::Gt => write!(f, ">"),
            ExprKind::Ge => write!(f, "≥"),
            ExprKind::Eq => write!(f, "="),
            ExprKind::Ne => write!(f, "≠"),
            ExprKind::Func(_) => todo!(),
            ExprKind::Iverson => todo!(),
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

    pub fn new_with_id(kind: StatementKind, id: u32) -> Self {
        Statement { id, kind }
    }

    pub fn clone_while(guard: Expr, body: Vec<Statement>, id: u32) -> Self {
        Statement {
            id,
            kind: StatementKind::While(guard, body),
        }
    }

    pub fn get_id(&self) -> u32 {
        self.id
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum StatementKind {
    Assignment(String, Expr),
    Sample(String),
    Bernoulli(String, Expr),
    Normal(String, Expr, Expr),
    Uniform(String, Expr, Expr),
    Branch(Expr, Vec<Statement>, Vec<Statement>),
    While(Expr, Vec<Statement>),
    Return(Expr),
    Observe(Expr),
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

pub type FnMap = HashMap<String, Func>;
