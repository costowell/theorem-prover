use std::fs;

use theorem_prover::{lexer::tokenize, parser::Parser};

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
    println!("{tokens:?}");
    let mut parser = Parser::new(tokens.iter().peekable());
    let expr = parser.parse().unwrap();
    println!("{expr}");
    let predicates = expr.get_predicates();

    println!();
    for predicate in &predicates {
        println!("{}: {}", predicate.ident, predicate.value)
    }
    println!();

    //Generate all possible binary values (ex: 00, 01, 10, 11 for 2 predicates)
    let iter = (0..(1 << predicates.len())).into_iter().map(|i| {
        (0..predicates.len())
            .into_iter()
            .map(move |x| ((i >> x) & 1) == 1)
            .collect::<Vec<bool>>()
    });

    (0..predicates.len()).for_each(|x| print!("{x} "));
    println!();

    let mut satisfiable = false;
    for state in iter {
        let result = expr.eval(state.as_slice());
        for v in state.iter() {
            print!("{} ", if *v { 1 } else { 0 })
        }
        if result {
            satisfiable = true;
        }
        println!("{}", result);
    }
    println!(
        "{expr} is {}",
        if satisfiable {
            "satisfiable"
        } else {
            "unsatisfiable"
        }
    );
}
