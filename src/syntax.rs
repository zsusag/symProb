use std::collections::HashMap;

use anyhow::Result;
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
        sig.push(self.ret_t.as_ref().unwrap());
        sig
    }

    pub fn get_body(&self) -> &Vec<Statement> {
        &self.body
    }
}
