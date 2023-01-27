use num::rational::Ratio;

use pest::iterators::Pairs;
use pest::pratt_parser::PrattParser;

use crate::expr::{Expr, ExprNode};
use crate::syntax::{ExprKind, Value};

// #[derive(pest_derive::Parser)]
// #[grammar = "psi_grammar.pest"]
// pub struct PsiParser;

// lazy_static::lazy_static! {
//     static ref PRATT_PARSER: PrattParser<Rule> = {
//         use pest::pratt_parser::{Assoc::*, Op};
//         use Rule::*;

//         // Precedence is defined lowest to highest
//         PrattParser::new()
//         // Addition and subtract have equal precedence
//             .op(Op::infix(lt, Left) | Op::infix(le, Left) | Op::infix(gt, Left) | Op::infix(ge, Left) | Op::infix(eq, Left) | Op::infix(ne, Left))
//             .op(Op::infix(add, Left) | Op::infix(subtract, Left))
//             .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
//             .op(Op::prefix(unary_minus) | Op::prefix(unary_not))
//             .op(Op::prefix(sqrt))
//     };
// }

// pub fn parse_expr(pairs: Pairs<Rule>) -> ExprNode {
//     PRATT_PARSER
//         .map_primary(|primary| match primary.as_rule() {
//             Rule::integer => ExprNode::new_leaf(ExprKind::Constant(Value::Num(Ratio::new(
//                 primary.as_str().parse::<i32>().unwrap(),
//                 1,
//             )))),
//             Rule::identifier => {
//                 ExprNode::new_leaf(ExprKind::Constant(Value::Var(primary.as_str().to_string())))
//             }
//             Rule::boolean => ExprNode::new_leaf(ExprKind::Constant(Value::Boolean(
//                 primary.as_str().parse().unwrap(),
//             ))),
//             Rule::expr => parse_expr(primary.into_inner()),
//             Rule::iverson => {
//                 ExprNode::new(ExprKind::Iverson, vec![parse_expr(primary.into_inner())])
//             }
//             rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
//         })
//         .map_infix(|lhs, op, rhs| {
//             let op = match op.as_rule() {
//                 Rule::add => ExprKind::Add,
//                 Rule::subtract => ExprKind::Sub,
//                 Rule::multiply => ExprKind::Mul,
//                 Rule::divide => ExprKind::Div,
//                 Rule::lt => ExprKind::Lt,
//                 Rule::le => ExprKind::Le,
//                 Rule::gt => ExprKind::Gt,
//                 Rule::ge => ExprKind::Ge,
//                 Rule::eq => ExprKind::Eq,
//                 Rule::ne => ExprKind::Ne,
//                 rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
//             };
//             if let (ExprKind::Constant(Value::Num(n1)), ExprKind::Constant(Value::Num(n2))) =
//                 (lhs.get_e(), rhs.get_e())
//             {
//                 match op {
//                     ExprKind::Add => ExprNode::new_leaf(ExprKind::Constant(Value::Num(
//                         n1.to_owned() + n2.to_owned(),
//                     ))),
//                     ExprKind::Sub => ExprNode::new_leaf(ExprKind::Constant(Value::Num(
//                         n1.to_owned() - n2.to_owned(),
//                     ))),
//                     ExprKind::Mul => ExprNode::new_leaf(ExprKind::Constant(Value::Num(
//                         n1.to_owned() * n2.to_owned(),
//                     ))),
//                     ExprKind::Div => ExprNode::new_leaf(ExprKind::Constant(Value::Num(
//                         n1.to_owned() / n2.to_owned(),
//                     ))),
//                     _ => unreachable!(),
//                 }
//             } else {
//                 ExprNode::new(op, vec![lhs, rhs])
//             }
//         })
//         .map_prefix(|op, mut rhs| match op.as_rule() {
//             Rule::unary_minus => {
//                 if let ExprKind::Constant(Value::Num(n)) = rhs.get_mut_e() {
//                     *n *= -1;
//                     rhs
//                 } else {
//                     ExprNode::new(
//                         ExprKind::Mul,
//                         vec![
//                             ExprNode::new_leaf(ExprKind::Constant(Value::Num(Ratio::new(-1, 1)))),
//                             rhs,
//                         ],
//                     )
//                 }
//             }
//             Rule::unary_not => ExprNode::new(ExprKind::Not, vec![rhs]),
//             Rule::sqrt => ExprNode::new(ExprKind::Sqrt, vec![rhs]),
//             _ => unreachable!(),
//         })
//         .parse(pairs)
// }

// pub fn parse_psi_output(mut pairs: Pairs<Rule>) -> Expr {
//     Expr::new(parse_expr(
//         pairs
//             .find(|p| matches!(p.as_rule(), Rule::expr))
//             .unwrap()
//             .into_inner(),
//     ))
// }
