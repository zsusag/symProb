use std::fs;

use num::rational::Ratio;
use pest::Parser;

use crate::{
    expr::{Expr, ExprNode},
    parser::{parse_expr, parse_file, ExprParser},
    syntax::{ExprKind, Value},
};

mod expr;
mod parser;
mod syntax;

fn main() {
    let f = fs::read_to_string("temp.txt").expect("Should have been able to read the file...");

    let data = ExprParser::parse(parser::Rule::file, &f).expect("unsuccessful parse");

    println!("{:#?}", parse_file(data));
}
