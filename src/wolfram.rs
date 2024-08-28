//! Translation of symbolic expressions into the [Wolfram](https://reference.wolfram.com/language/).

use std::fmt::Display;

use crate::expr::ExprNode;
use crate::syntax::{ExprKind, Value};

/// A newtype wrapper around an [`Expr`] whose `Display` implementation prints the symbolic
/// expression in Wolfram language syntax.
#[derive(Debug)]
pub struct WolframExpr<'a>(&'a ExprNode);

impl<'a> WolframExpr<'a> {
    pub fn new(e: &'a ExprNode) -> Self {
        Self(e)
    }
}

impl<'a> From<&'a ExprNode> for WolframExpr<'a> {
    fn from(value: &'a ExprNode) -> Self {
        Self::new(value)
    }
}

impl<'a> Display for WolframExpr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Get references to the two inner children
        let c0 = self.0.children.get(0);
        let c1 = self.0.children.get(1);
        match &self.0.e {
            ExprKind::Constant(val) => match val {
                Value::Num(x) => {
                    if x.is_integer() {
                        write!(f, "{val}")
                    } else {
                        // `Rational32`'s `Display` implementation prints the numerator, a `/`, and
                        // then the denominator. We surround rational numbers with parentheses to
                        // avoid ambiguity. It is also shorter than using the fully qualified
                        // `Divide[.,.]` operation in Wolfram.
                        write!(f, "({val})")
                    }
                }
                Value::Boolean(b) => {
                    if *b {
                        write!(f, "True")
                    } else {
                        write!(f, "False")
                    }
                }
                Value::Var(s) => write!(f, "{s}"),
            },
            ExprKind::Add => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}+{c1}")
            }
            ExprKind::Sub => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}-{c1}")
            }
            ExprKind::Mul => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}*{c1}")
            }
            ExprKind::Div => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "Divide[{c0},{c1}]")
            }
            ExprKind::Sqrt => {
                let c0: WolframExpr = c0.unwrap().into();
                write!(f, "Sqrt[{c0}]")
            }
            ExprKind::And => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "And[{c0},{c1}]")
            }
            ExprKind::Or => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "Or[{c0},{c1}]")
            }
            ExprKind::Not => {
                let c0: WolframExpr = c0.unwrap().into();
                write!(f, "Not[{c0}]")
            }
            ExprKind::Lt => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}<{c1}")
            }
            ExprKind::Le => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}<={c1}")
            }
            ExprKind::Gt => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}>{c1}")
            }
            ExprKind::Ge => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}>={c1}")
            }
            ExprKind::Eq => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}=={c1}")
            }
            ExprKind::Ne => {
                let c0: WolframExpr = c0.unwrap().into();
                let c1: WolframExpr = c1.unwrap().into();
                write!(f, "{c0}!={c1}")
            }
            ExprKind::Func(_) => {
                panic!("function calls cannot be currently printed as Wolfram expressions")
            }
            ExprKind::Iverson => {
                let c0: WolframExpr = c0.unwrap().into();
                write!(f, "Boole[{c0}]")
            }
            ExprKind::Exp => {
                let c0: WolframExpr = c0.unwrap().into();
                write!(f, "Exp[{c0}]")
            }
            ExprKind::Negate => {
                let c0: WolframExpr = c0.unwrap().into();
                write!(f, "-{c0}")
            }
            ExprKind::Square => {
                let c0: WolframExpr = c0.unwrap().into();
                write!(f, "Power[{c0},2]")
            }
            ExprKind::Pi => write!(f, "Pi"),
        }
    }
}
