use std::{cell::RefCell, collections::HashMap, fmt::Display, fs};

use anyhow::bail;
use itertools::{EitherOrBoth, Itertools};
use nalgebra::DMatrix;
use theorem_prover::{
    lexer::tokenize,
    parser::{Equation, Parser, PredicateValue},
};

#[derive(Debug, Clone)]
pub struct LinearSystem {
    vars: Vec<Option<String>>,
    vars_to_index: HashMap<Option<String>, usize>,
    matrix: RefCell<DMatrix<f64>>,
}

impl<'a> LinearSystem {
    pub fn from_equations(eqns: &'a [&'a Equation]) -> Self {
        let mut vars_to_index: HashMap<Option<String>, usize> = HashMap::new();
        let mut vars: Vec<Option<String>> = Vec::new();
        let mut rows: Vec<Vec<f64>> = Vec::new();

        // Turn equations into matrix
        for eqn in eqns {
            let eqn = eqn.to_equals_zero();
            let mut row: Vec<f64> = Vec::new();
            for term in eqn {
                let pos = if let Some(pos) = vars_to_index.get(&term.name) {
                    *pos
                } else {
                    let i = vars.len();
                    vars.push(term.name.clone());
                    vars_to_index.insert(term.name, i);
                    i
                };
                if pos >= row.len() {
                    row.resize(pos + 1, 0.0);
                }
                row[pos] = term.coeff;
            }
            rows.push(row);
        }

        let matrix = DMatrix::from_row_iterator(
            rows.len(),
            vars.len(),
            rows.into_iter().flat_map(|mut x| {
                x.resize(vars.len(), 0.0);
                x
            }),
        );
        Self {
            matrix: RefCell::new(matrix),
            vars,
            vars_to_index,
        }
    }

    /// https://rosettacode.org/wiki/Reduced_row_echelon_form
    pub fn calc_rref(&self) {
        let mut matrix = self.matrix.borrow_mut();
        let mut lead = 0;
        'main: for r in 0..matrix.nrows() {
            if lead >= matrix.ncols() {
                break;
            }
            let mut i = r;
            while matrix[(i, lead)] == 0.0 {
                i += 1;
                if i == matrix.nrows() {
                    i = r;
                    lead += 1;
                    if matrix.ncols() == lead {
                        break 'main;
                    }
                }
            }
            matrix.swap_rows(i, r);
            let mut lv = matrix[(r, lead)];
            for val in matrix.row_mut(r) {
                *val /= lv
            }
            for i in 0..matrix.nrows() {
                if i != r {
                    lv = matrix[(i, lead)];
                    let r_row = matrix.row(r).clone_owned();
                    for (iv, rv) in matrix.row_mut(i).into_iter().zip(r_row.into_iter()) {
                        *iv -= lv * rv;
                    }
                }
            }
        }
    }

    pub fn is_consistent(&self) -> bool {
        self.calc_rref();

        let Some(ci) = self.vars_to_index.get(&None) else {
            return true;
        };
        !self
            .matrix
            .borrow()
            .row_iter()
            .filter(|x| x[*ci] != 0.0)
            .any(|x| !x.iter().enumerate().any(|(i, x)| i != *ci && *x != 0.0))
    }

    pub fn add_equation_limited(&self, eqn: &Equation) -> anyhow::Result<()> {
        let eqn = eqn.to_equals_zero();
        let mut row: Vec<f64> = vec![0.0; self.vars.len()];
        for term in eqn {
            let Some(pos) = self.vars_to_index.get(&term.name) else {
                bail!("can't insert extra var")
            };
            row[*pos] = term.coeff;
        }
        self.matrix.replace_with(|m| {
            let mut m = m.clone().insert_row(m.nrows(), 0.0);
            m.row_mut(m.nrows() - 1).copy_from_slice(&row);
            m
        });
        Ok(())
    }
}

impl Display for LinearSystem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for var in self.vars.iter() {
            f.write_str(var.as_deref().unwrap_or(""))?;
            f.write_str(" ")?;
        }
        Display::fmt(&self.matrix.borrow(), f)
    }
}

// NOTE: This doesn't account for transposed columns where the coresponding variables' order is different.
//       This saves on performance
impl PartialEq for LinearSystem {
    fn eq(&self, other: &Self) -> bool {
        self.vars == other.vars
            && !self
                .matrix
                .borrow()
                .row_iter()
                .zip_longest(other.matrix.borrow().row_iter())
                .any(|x| {
                    if let EitherOrBoth::Both(a, b) = x {
                        a != b
                    } else {
                        false
                    }
                })
    }
}

fn is_consistent<'a>(eqns: &'a [&'a Equation], neqns: &'a [&'a Equation]) -> bool {
    let system = LinearSystem::from_equations(eqns);
    if !system.is_consistent() {
        return false;
    }

    for neqn in neqns {
        let new_system = system.clone();
        if new_system.add_equation_limited(neqn).is_err() {
            continue;
        }
        new_system.calc_rref();

        if system == new_system {
            return false;
        }
    }

    true
}

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
    let mut parser = Parser::new(tokens.iter().peekable());
    let expr = parser.parse().unwrap();
    let predicates = expr.get_predicates();

    println!();
    for predicate in &predicates {
        println!("{}: {}", predicate.ident, predicate.value)
    }
    println!();

    //Generate all possible boolean arrays (ex: 00, 01, 10, 11 for 2 predicates)
    let iter = (0..(1 << predicates.len())).map(|i| {
        (0..predicates.len())
            .map(move |x| ((i >> x) & 1) == 1)
            .collect::<Vec<bool>>()
    });

    (0..predicates.len()).for_each(|x| print!("{x} "));
    println!();

    let mut satisfiable = false;
    let mut prooved = true;
    for state in iter {
        let result = expr.eval(state.as_slice());

        for v in state.iter() {
            print!("{} ", if *v { 1 } else { 0 })
        }

        if result {
            let mut eqns = vec![];
            let mut neqns = vec![];

            for (predicate, t) in predicates.iter().zip(state) {
                if let PredicateValue::Equation(eq) = &predicate.value {
                    if t {
                        eqns.push(eq);
                    } else {
                        neqns.push(eq);
                    }
                }
            }

            let consistent = is_consistent(eqns.as_slice(), neqns.as_slice());
            if consistent {
                satisfiable = true;
            } else {
                prooved = false;
            }
            println!("{consistent}")
        } else {
            println!("trivially false")
        }
    }
    println!(
        "{expr} is {}",
        if satisfiable {
            "satisfiable"
        } else {
            "unsatisfiable"
        }
    );
    println!(
        "{expr} is {}",
        if prooved { "prooven" } else { "not prooven" }
    );
}
