use num::Rational32;

use crate::expr::Expr;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Type {
    Real,
    Bool,
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Value {
    Num(Rational32),
    Boolean(bool),
    Var(String),
}

#[derive(Debug, PartialEq, Clone, Eq)]
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
    Func(String),
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
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
    inputs: Vec<(String, Type)>,
    body: Vec<Statement>,
}

impl Func {
    pub fn new(name: String, inputs: Vec<(String, Type)>, body: Vec<Statement>) -> Self {
        Func { name, inputs, body }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}
