use std::collections::HashMap;

use num::rational::Ratio;
use num::{FromPrimitive, Rational32};
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;

use crate::expr::{Expr, ExprNode};
use crate::syntax::{ExprKind, Func, Statement, StatementKind, Type, Value};

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct ExprParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
        // Addition and subtract have equal precedence
            .op(Op::infix(or, Left))
            .op(Op::infix(and, Left))
            .op(Op::infix(lt, Left) | Op::infix(le, Left) | Op::infix(gt, Left) | Op::infix(ge, Left) | Op::infix(eq, Left) | Op::infix(ne, Left))
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::prefix(unary_minus) | Op::prefix(unary_not))
    };
}

pub fn parse_type(pair: Pair<Rule>) -> Type {
    match pair.as_str() {
        "Real" => Type::Real,
        "Boolean" => Type::Bool,
        _ => unreachable!(),
    }
}

pub fn parse_expr(pairs: Pairs<Rule>) -> ExprNode {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::fn_call => {
                let mut inner = primary.into_inner();
                // A fn_call has to have an identifer, and optionally a list of exprs.
                let fn_name = inner.next().unwrap().as_str().to_string();
                let arguments: Vec<ExprNode> = inner.map(|p| parse_expr(p.into_inner())).collect();
                ExprNode::new(ExprKind::Func(fn_name), arguments)
            }
            Rule::integer => ExprNode::new_leaf(ExprKind::Constant(Value::Num(Ratio::new(
                primary.as_str().parse::<i32>().unwrap(),
                1,
            )))),
            Rule::decimal => {
                let poss_rat_val = Rational32::from_f64(primary.as_str().parse::<f64>().unwrap());
                match poss_rat_val {
                    Some(rat_val) => ExprNode::new_leaf(ExprKind::Constant(Value::Num(rat_val))),
                    None => panic!("Unable to convert floaing point number into a Rational32"),
                }
            }
            Rule::identifier => {
                ExprNode::new_leaf(ExprKind::Constant(Value::Var(primary.as_str().to_string())))
            }
            Rule::boolean => ExprNode::new_leaf(ExprKind::Constant(Value::Boolean(
                primary.as_str().parse().unwrap(),
            ))),
            Rule::expr => parse_expr(primary.into_inner()),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => ExprKind::Add,
                Rule::subtract => ExprKind::Sub,
                Rule::multiply => ExprKind::Mul,
                Rule::divide => ExprKind::Div,
                Rule::and => ExprKind::And,
                Rule::or => ExprKind::Or,
                Rule::lt => ExprKind::Lt,
                Rule::le => ExprKind::Le,
                Rule::gt => ExprKind::Gt,
                Rule::ge => ExprKind::Ge,
                Rule::eq => ExprKind::Eq,
                Rule::ne => ExprKind::Ne,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };
            if let (ExprKind::Constant(Value::Num(n1)), ExprKind::Constant(Value::Num(n2))) =
                (lhs.get_e(), rhs.get_e())
            {
                match op {
                    ExprKind::Add => ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                        n1.to_owned() + n2.to_owned(),
                    ))),
                    ExprKind::Sub => ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                        n1.to_owned() - n2.to_owned(),
                    ))),
                    ExprKind::Mul => ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                        n1.to_owned() * n2.to_owned(),
                    ))),
                    ExprKind::Div => ExprNode::new_leaf(ExprKind::Constant(Value::Num(
                        n1.to_owned() / n2.to_owned(),
                    ))),
                    _ => unreachable!(),
                }
            } else {
                ExprNode::new(op, vec![lhs, rhs])
            }
        })
        .map_prefix(|op, mut rhs| match op.as_rule() {
            Rule::unary_minus => {
                if let ExprKind::Constant(Value::Num(n)) = rhs.get_mut_e() {
                    *n *= -1;
                    rhs
                } else {
                    ExprNode::new(
                        ExprKind::Mul,
                        vec![
                            ExprNode::new_leaf(ExprKind::Constant(Value::Num(Ratio::new(-1, 1)))),
                            rhs,
                        ],
                    )
                }
            }
            Rule::unary_not => ExprNode::new(ExprKind::Not, vec![rhs]),
            _ => unreachable!(),
        })
        .parse(pairs)
}

// Each fn_def consists of the following:
// 1) identifier (name of the function)
// 2) A (possibly empty) parameter list, which are pairs of identifiers and types
// 3) A statement list
pub fn parse_func(mut pairs: Pairs<Rule>, mut statement_counter: &mut u32) -> Func {
    // Get the name of the function
    let name = pairs.next().unwrap().as_str().to_string();
    let inputs: Vec<(String, Type)> = pairs
        .next()
        .unwrap()
        .into_inner()
        .map(|p| {
            let mut inner_rules = p.into_inner();
            let var_name = inner_rules.next().unwrap().as_str().to_string();
            let t = parse_type(inner_rules.next().unwrap());
            (var_name, t)
        })
        .collect();
    let poss_t = pairs.next().unwrap();
    let (ret_t, body) = match poss_t.as_rule() {
        Rule::t => (Some(parse_type(poss_t)), pairs.next().unwrap()),
        Rule::statement_list => (None, poss_t),
        _ => unreachable!(),
    };
    let body: Vec<Statement> = body
        .into_inner()
        .map(|s| parse_statement(s, &mut statement_counter))
        .collect();
    Func::new(name, inputs, body, ret_t)
}

pub fn parse_statement(pair: Pair<Rule>, mut statement_counter: &mut u32) -> Statement {
    match pair.as_rule() {
        Rule::assignment => {
            let mut inner_rules = pair.into_inner();

            let var = inner_rules.next().unwrap().as_str().to_string();
            let e = parse_expr(inner_rules.next().unwrap().into_inner());

            Statement::new(
                StatementKind::Assignment(var, Expr::new(e)),
                &mut statement_counter,
            )
        }
        Rule::sample => {
            let mut inner_rules = pair.into_inner();

            let var = inner_rules.next().unwrap().as_str().to_string();
            Statement::new(StatementKind::Sample(var), &mut statement_counter)
        }
        Rule::bern => {
            let mut inner_rules = pair.into_inner();

            let var = inner_rules.next().unwrap().as_str().to_string();
            let e = parse_expr(inner_rules.next().unwrap().into_inner());
            let ret = Statement::new(
                StatementKind::Bernoulli(var, Expr::new(e)),
                &mut statement_counter,
            );
            *statement_counter += 3; // We expand the bern statment later into 4 statements
            ret
        }
        Rule::normal => {
            let mut inner_rules = pair.into_inner();

            let var = inner_rules.next().unwrap().as_str().to_string();
            let mean = parse_expr(inner_rules.next().unwrap().into_inner());
            let variance = parse_expr(inner_rules.next().unwrap().into_inner());
            Statement::new(
                StatementKind::Normal(var, Expr::new(mean), Expr::new(variance)),
                &mut statement_counter,
            )
        }
        Rule::branch => {
            let mut inner_rules = pair.into_inner();

            let cond = Expr::new(parse_expr(inner_rules.next().unwrap().into_inner()));
            let true_branch: Vec<Statement> = inner_rules
                .next()
                .unwrap()
                .into_inner()
                .map(|s| parse_statement(s, &mut statement_counter))
                .collect();
            let false_branch: Vec<Statement> = inner_rules
                .next()
                .unwrap()
                .into_inner()
                .map(|s| parse_statement(s, &mut statement_counter))
                .collect();

            Statement::new(
                StatementKind::Branch(cond, true_branch, false_branch),
                &mut statement_counter,
            )
        }
        Rule::while_st => {
            let mut inner_rules = pair.into_inner();

            let cond = Expr::new(parse_expr(inner_rules.next().unwrap().into_inner()));
            let body: Vec<Statement> = inner_rules
                .next()
                .unwrap()
                .into_inner()
                .map(|s| parse_statement(s, &mut statement_counter))
                .collect();

            Statement::new(StatementKind::While(cond, body), &mut statement_counter)
        }
        Rule::return_st => {
            let ret_expr = Expr::new(parse_expr(pair.into_inner().next().unwrap().into_inner()));
            Statement::new(StatementKind::Return(ret_expr), &mut statement_counter)
        }
        Rule::observe => {
            let mut inner_rules = pair.into_inner();
            let cond = Expr::new(parse_expr(inner_rules.next().unwrap().into_inner()));
            Statement::new(StatementKind::Observe(cond), &mut statement_counter)
        }
        _ => unreachable!(),
    }
}

pub fn parse_file(pairs: Pairs<Rule>) -> HashMap<String, Func> {
    let mut statement_counter: u32 = 0;
    pairs
        .filter_map(|p| match p.as_rule() {
            Rule::fn_def => {
                let f = parse_func(p.into_inner(), &mut statement_counter);
                Some((f.get_name().to_string(), f))
            }
            _ => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use crate::expr::Expr;

    use super::*;

    #[test]
    fn single_op_ints() {
        let input = "5 + 2";
        let actual = Expr::new(ExprNode::new_leaf(ExprKind::Constant(Value::Num(
            Ratio::new(7, 1),
        ))));
        let res = match ExprParser::parse(Rule::equation, input) {
            Ok(mut pairs) => Expr::new(parse_expr(pairs.next().unwrap().into_inner())),
            Err(_) => unreachable!(),
        };
        assert_eq!(actual, res)
    }

    #[test]
    fn compress_unary_minus() {
        let input = "-3 + 2";
        let actual = Expr::new(ExprNode::new_leaf(ExprKind::Constant(Value::Num(
            Ratio::new(-1, 1),
        ))));
        let res = match ExprParser::parse(Rule::equation, input) {
            Ok(mut pairs) => Expr::new(parse_expr(pairs.next().unwrap().into_inner())),
            Err(_) => unreachable!(),
        };
        assert_eq!(actual, res)
    }

    #[test]
    fn expr_with_vars() {
        let input = "5 + x_0 * 2";
        let actual = Expr::new(ExprNode::new(
            ExprKind::Add,
            vec![
                ExprNode::new_leaf(ExprKind::Constant(Value::Num(Ratio::new(5, 1)))),
                ExprNode::new(
                    ExprKind::Mul,
                    vec![
                        ExprNode::new_leaf(ExprKind::Constant(Value::Var("x_0".to_string()))),
                        ExprNode::new_leaf(ExprKind::Constant(Value::Num(Ratio::new(2, 1)))),
                    ],
                ),
            ],
        ));
        let res = match ExprParser::parse(Rule::equation, input) {
            Ok(mut pairs) => Expr::new(parse_expr(pairs.next().unwrap().into_inner())),
            Err(_) => unreachable!(),
        };
        assert_eq!(actual, res)
    }

    #[test]
    fn bool_ops() {
        let input = "true || x && false";
        let actual = Expr::new(ExprNode::new(
            ExprKind::Or,
            vec![
                ExprNode::new_leaf(ExprKind::Constant(Value::Boolean(true))),
                ExprNode::new(
                    ExprKind::And,
                    vec![
                        ExprNode::new_leaf(ExprKind::Constant(Value::Var("x".to_string()))),
                        ExprNode::new_leaf(ExprKind::Constant(Value::Boolean(false))),
                    ],
                ),
            ],
        ));
        let res = match ExprParser::parse(Rule::equation, input) {
            Ok(mut pairs) => Expr::new(parse_expr(pairs.next().unwrap().into_inner())),
            Err(_) => unreachable!(),
        };
        assert_eq!(actual, res)
    }

    #[test]
    fn unary_not() {
        let input = "!(5 < !x)";
        let actual = Expr::new(ExprNode::new(
            ExprKind::Not,
            vec![ExprNode::new(
                ExprKind::Lt,
                vec![
                    ExprNode::new_leaf(ExprKind::Constant(Value::Num(Ratio::new(5, 1)))),
                    ExprNode::new(
                        ExprKind::Not,
                        vec![ExprNode::new_leaf(ExprKind::Constant(Value::Var(
                            "x".to_string(),
                        )))],
                    ),
                ],
            )],
        ));
        let res = match ExprParser::parse(Rule::equation, input) {
            Ok(mut pairs) => Expr::new(parse_expr(pairs.next().unwrap().into_inner())),
            Err(_) => unreachable!(),
        };
        assert_eq!(actual, res)
    }
}
