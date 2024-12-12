use std::{collections::HashMap, fs};

use itertools::Itertools;
use theorem_prover::{
    lexer::{tokenize, Token},
    parser::Parser,
};

fn main() {
    let s = fs::read_to_string(
        std::env::args()
            .nth(1)
            .expect("First argument must be a path to a file"),
    )
    .expect("Failed to read from file")
    .trim()
    .to_string();
    let tokens = tokenize(s).unwrap();
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
    let mut satisfiable = false;
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
        if result {
            satisfiable = true;
        }
        println!("{}", result);
    }
    println!();
    println!(
        "{expr} is {}",
        if satisfiable {
            "satisfiable"
        } else {
            "unsatisfiable"
        }
    );
}
