use std::{collections::HashMap, ops::Neg};

use itertools::Itertools;
use nalgebra::DMatrix;

/// Once we understand which boolean values satisfy an expression (i.e. "p & q" has p = true, q = true satisfy it)
/// we must then determine if these boolean values contradict.
/// i.e. "(x = 1) & (x = 2)", although if both were true, the statement is satisfied, both can't be true, and thus the statement is false
///
/// The intention of this module/binary is to try and determine contradictory statements phrased as equations
/// We will constrain ourselves to the set of integers and all operations closed under them
/// - Multiplication
/// - Addition
/// - Subtraction (tivial, mult + add)
/// - Exponent
/// - Modulo (seems to be the most challenging)
///
/// My first approach was to arrange the equations in a matrix and solve for the REF until there is a contradiction
/// However, negated equations complicated things for a few hours, until I developed the following approach.
///
/// 1. Arrange equations in a matrix
/// 2. Solve RREF of matrix
///   - If zero row = non zero value, contradiction, not satisfiable
/// 3. Extract equivalence from rows
///   - ExA: [1 -1 | 0] -> a = b
///
///   - ExB: [1  0 | 0] -> a = 0
///          [0  1 | 1] -> b = 1
/// 4. Build equivalence classes
///   - ExA: C1 = {a, b}
///   - ExB: C1 = {a, 0}, C2 = {b, 1}
/// 5. Generate all possible equivalent negated equations until LHS = RHS
///   - If none exist, no contradiction, therefore satisfiable
///
/// At present, none of this is implemented, so I could very well be wrong.
/// For now, this preface will serve as a way to organize my thoughts and demonstrate my thought process.

/// Represents some variable and a coefficient
#[derive(Debug)]
pub struct Term<'a> {
    pub coeff: i64,
    pub var: &'a str,
}

/// Represents a sum of terms
pub type LinearExpression<'a> = Vec<Term<'a>>;

/// An equation is something of the form "x = as + bt + ... + cu"
/// where a, b, c are constants and s, t, u are variables
#[derive(Debug)]
pub struct Equation<'a> {
    pub lhs: LinearExpression<'a>,
    pub rhs: LinearExpression<'a>,
}

impl<'a> Neg for Term<'a> {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            coeff: -self.coeff,
            var: self.var,
        }
    }
}

impl<'a> Equation<'a> {
    /// Consumes the expression and outputs an expression where the lhs has all terms
    pub fn equals_zero(self) -> LinearExpression<'a> {
        // term_side is where the terms are going, zero_side is where no terms should be
        let (mut term_side, zero_side) = if self.lhs.len() > self.rhs.len() {
            (self.lhs, self.rhs)
        } else {
            (self.rhs, self.lhs)
        };
        let mut new_terms = zero_side.into_iter().map(|x| -x).collect();
        term_side.append(&mut new_terms);
        term_side
    }
}

/// https://rosettacode.org/wiki/Reduced_row_echelon_form
fn rref(mut matrix: DMatrix<f64>) -> DMatrix<f64> {
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
    matrix
}

fn main() {
    let eqn1 = Equation {
        lhs: vec![Term { coeff: 1, var: "a" }],
        rhs: vec![Term { coeff: 2, var: "b" }],
    };
    let eqn2 = Equation {
        lhs: vec![Term { coeff: 1, var: "b" }],
        rhs: vec![Term { coeff: 2, var: "a" }],
    };
    let eqn3 = Equation {
        lhs: vec![Term { coeff: 1, var: "a" }],
        rhs: vec![Term { coeff: 2, var: "" }],
    };

    let mut vars: HashMap<&str, usize> = HashMap::new();
    let mut rows: Vec<Vec<f64>> = Vec::new();
    let mut i: usize = 0;

    for eqn in [eqn1, eqn2, eqn3] {
        let eqn = eqn.equals_zero();
        println!("Eqn: {eqn:?}");
        let mut row: Vec<f64> = Vec::new();
        for term in eqn {
            let pos = if let Some(pos) = vars.get(term.var) {
                *pos
            } else {
                vars.insert(term.var, i);
                i += 1;
                i - 1
            };
            if pos >= row.len() {
                row.resize(pos + 1, 0.0);
            }
            row[pos] = term.coeff as f64;
        }
        println!("Row: {row:?}");
        rows.push(row);
    }
    let matrix = DMatrix::from_row_iterator(
        rows.len(),
        i,
        rows.into_iter()
            .map(|mut x| {
                x.resize(i, 0.0);
                x
            })
            .flatten(),
    );
    println!(
        "{:?}",
        vars.iter()
            .sorted_by_key(|x| x.1)
            .map(|x| x.0)
            .collect::<Vec<_>>()
    );
    print!("{}", matrix);
    let matrix = rref(matrix);
    print!("{}", matrix);
}
