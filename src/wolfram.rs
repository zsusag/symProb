//! Translation of symbolic expressions into the [Wolfram](https://reference.wolfram.com/language/).

use std::fmt::Display;

use crate::expr::ExprNode;
use crate::syntax::{ExprKind, Value};

/// A newtype wrapper around an [`Expr`] whose `Display` implementation prints the symbolic
/// expression in Wolfram language syntax.
#[derive(Debug)]
pub struct WolframExpr(ExprNode);

impl WolframExpr {
    pub fn new(e: ExprNode) -> Self {
        Self(e)
    }
}

impl Display for WolframExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.e {
            ExprKind::Constant(val) => match val {
                Value::Num(x) => {
                    if x.is_integer() {
                        write!(f, "{val}")
                    } else {
                        // `Rational32`'s `Display` implementation prints the numerator, a `/`, and
                        // then the denominator. We surround rational numbers with parentheses to
                        // avoid ambiguity.
                        write!(f, "({val})")
                    }
                }
                Value::Boolean(b) => todo!(),
                Value::Var(String) => todo!(),
            },
            ExprKind::Add => todo!(),
            ExprKind::Sub => todo!(),
            ExprKind::Mul => todo!(),
            ExprKind::Div => todo!(),
            ExprKind::Sqrt => todo!(),
            ExprKind::And => todo!(),
            ExprKind::Or => todo!(),
            ExprKind::Not => todo!(),
            ExprKind::Lt => todo!(),
            ExprKind::Le => todo!(),
            ExprKind::Gt => todo!(),
            ExprKind::Ge => todo!(),
            ExprKind::Eq => todo!(),
            ExprKind::Ne => todo!(),
            ExprKind::Func(_) => todo!(),
            ExprKind::Iverson => todo!(),
        }
    }
}
