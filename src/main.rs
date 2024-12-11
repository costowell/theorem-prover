mod lexer;
mod parser;

use std::collections::HashMap;

use itertools::Itertools;
use lexer::{tokenize, Token};
use parser::Parser;

const EXPR: &'static str = "(!A & B) -> !(A | !B)";

fn main() {
    let tokens = tokenize(EXPR.to_string()).unwrap();
    let mut iter = tokens.iter().peekable();
    let mut parser = Parser::new(&mut iter);
    let expr = parser.parse().unwrap();
    let predicates: Vec<&String> = tokens
        .iter()
        .filter_map(|x| {
            if let Token::Predicate(x) = x {
                Some(x)
            } else {
                None
            }
        })
        .unique()
        .collect();
    for predicate in &predicates {
        print!("{predicate} ")
    }
    println!();
    let mut prooved = true;
    for n in 0..(2_usize.pow(predicates.len() as u32)) {
        let values_iter = predicates
            .iter()
            .enumerate()
            .map(|(i, x)| (x.as_str(), (n >> i) & 1 == 1));
        for v in values_iter.clone() {
            print!("{} ", if v.1 { 1 } else { 0 })
        }

        let map: HashMap<&str, bool> = HashMap::from_iter(values_iter);
        let result = expr.eval(&map);
        if !result {
            prooved = false;
        }
        println!("{}", result);
    }
    println!();
    println!(
        "{expr} is {}",
        if prooved {
            "true"
        } else {
            "not true for all cases"
        }
    );
}
