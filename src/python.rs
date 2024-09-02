//! Translation of symbolic expressions into the [Python](https://reference.python.com/language/).
use std::fmt::Display;

use serde::Serialize;

use crate::executor_state::Dist;
use crate::expr::{Expr, ExprNode, PreExpectationIntegrand};
use crate::syntax::{ExprKind, Value};

/// A newtype wrapper around an [`Expr`] whose `Display` implementation prints the symbolic
/// expression in Python language syntax.
#[derive(Debug)]
pub struct PythonExpr<'a>(&'a ExprNode);

impl<'a> PythonExpr<'a> {
    pub fn new(e: &'a ExprNode) -> Self {
        Self(e)
    }
}

impl<'a> From<&'a ExprNode> for PythonExpr<'a> {
    fn from(value: &'a ExprNode) -> Self {
        Self::new(value)
    }
}

impl<'a> Display for PythonExpr<'a> {
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
                        // `Divide[.,.]` operation in Python.
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
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "{c0}+{c1}")
            }
            ExprKind::Sub => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(
                    f,
                    "{c0}-{}",
                    if c1.0.needs_parens() {
                        format!("({})", c1)
                    } else {
                        c1.to_string()
                    }
                )
            }
            ExprKind::Mul => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(
                    f,
                    "{}*{}",
                    if c0.0.needs_parens() {
                        format!("({})", c0)
                    } else {
                        c0.to_string()
                    },
                    if c1.0.needs_parens() {
                        format!("({})", c1)
                    } else {
                        c1.to_string()
                    }
                )
            }
            ExprKind::Div => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f,
                    "{}/{}",
                    if c0.0.needs_parens() {
                        format!("({})", c0)
                    } else {
                        c0.to_string()
                    },
                    format!("({})", c1)
                )
            }
            ExprKind::Sqrt => {
                let c0: PythonExpr = c0.unwrap().into();
                write!(f, "sqrt({c0})")
            }
            ExprKind::And => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "{c0} and {c1}")
            }
            ExprKind::Or => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "{c0} or {c1}")
            }
            ExprKind::Not => {
                let c0: PythonExpr = c0.unwrap().into();
                write!(f, "not ({c0})")
            }
            ExprKind::Lt => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "({c0})<({c1})")
            }
            ExprKind::Le => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "({c0})<=({c1})")
            }
            ExprKind::Gt => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "({c0})>({c1})")
            }
            ExprKind::Ge => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "({c0})>=({c1})")
            }
            ExprKind::Eq => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "({c0})==({c1})")
            }
            ExprKind::Ne => {
                let c0: PythonExpr = c0.unwrap().into();
                let c1: PythonExpr = c1.unwrap().into();
                write!(f, "({c0})!=({c1})")
            }
            ExprKind::Func(_) => {
                panic!("function calls cannot be currently printed as Python expressions")
            }
            ExprKind::Iverson => {
                let c0: PythonExpr = c0.unwrap().into();
                write!(f, "(1 if ({c0}) else 0)")
            }
            ExprKind::Exp => {
                let c0: PythonExpr = c0.unwrap().into();
                write!(f, "exp({c0})")
            }
            ExprKind::Negate => {
                let c0: PythonExpr = c0.unwrap().into();
                write!(f,"-({c0})")
            }
            ExprKind::Square => {
                let c0: PythonExpr = c0.unwrap().into();
                write!(f, "({c0})**2")
            }
            ExprKind::Pi => write!(f, "pi"),
        }
    }
}

fn gaussian_factor(var: &str) -> Expr {
    let var_squared = ExprNode::new(
        ExprKind::Square,
        vec![ExprNode::new_sample_var(var.to_string())],
    );
    let neg_var_squared = ExprNode::new(ExprKind::Negate, vec![var_squared]);
    let two = ExprNode::new_leaf(ExprKind::Constant(Value::Num(2.into())));
    let neg_var_squared_div_two = ExprNode::new(ExprKind::Div, vec![neg_var_squared, two.clone()]);
    let numer = ExprNode::new(ExprKind::Exp, vec![neg_var_squared_div_two]);

    let pi = ExprNode::new_leaf(ExprKind::Pi);
    let two_pi = ExprNode::new(ExprKind::Mul, vec![two, pi]);
    let denom = ExprNode::new(ExprKind::Sqrt, vec![two_pi]);

    let root = ExprNode::new(ExprKind::Div, vec![numer, denom]);
    Expr::new(root)
}

#[derive(Debug)]
struct IntegralParam {
    var: String,
    lower: i32,
    upper: i32,
}

impl IntegralParam {
    pub fn new(var: String, lower: i32, upper: i32) -> Self {
        Self { var, lower, upper }
    }
}

impl Display for IntegralParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { var, lower:_, upper:_ } = self;
        write!(f, "{var}")
    }
}

#[derive(Debug)]
pub struct PythonPreExpectation {
    integrand: Expr,
    params: Vec<IntegralParam>,
}

impl PythonPreExpectation {
    pub fn new<'a, I>(pre_exp_int: PreExpectationIntegrand, psvs: I, integral_bounds: i32) -> Self
    where
        I: Iterator<Item = (&'a String, &'a Dist)> + Clone,
    {
        let gaussian_factors: Vec<Expr> = psvs
            .clone()
            .filter_map(|(var, dist)| match dist {
                Dist::Normal => Some(gaussian_factor(var)),
                Dist::Uniform => None,
            })
            .collect();

        // Product is guaranteed to be non-empty so we can safely `unwrap` here.
        let integrand =
            crate::expr::product(std::iter::once(pre_exp_int.0).chain(gaussian_factors)).unwrap();

        let integration_params = psvs
            .map(|(var, dist)| match dist {
                Dist::Uniform => IntegralParam::new(var.to_string(), 0, 1),
                Dist::Normal => {
                    IntegralParam::new(var.to_string(), -integral_bounds, integral_bounds)
                }
            })
            .collect();

        Self {
            integrand,
            params: integration_params,
        }
    }
}

impl Display for PythonPreExpectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { integrand, params:_ } = self;
        write!(f,"{}",PythonExpr::new(&integrand.root))
    }
}

/// Use [`PythonPreExpectation`]'s `Display` implementation to serialize [`PythonPreExpectation`].
impl Serialize for PythonPreExpectation {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}
