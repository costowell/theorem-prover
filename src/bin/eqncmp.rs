use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    ops::Neg,
};

use anyhow::bail;
use itertools::{EitherOrBoth, Itertools};
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
///
///
/// Dec 14, 2024: Possible Break-through
///
/// After thinking about it for a bit, I think the matrix is enough and equivalence classes are redundant.
/// Say we have a few equations like so:
/// a + c = 0
/// b + c = 0
/// a != b
///
/// Using the method I had before, my equivalence classes would like
/// C1 = { a + c, b + c, 0 }
///
/// And then when "generating possible negated equations", there is nothing said to be equivalent to a or b yet.
/// This is a big hole in my process that I can only imagine gets wider the more cases you throw at it.
///
/// The solution? Just use a matrix. If we assert "a != b", then logically, "a == b" should not be consistent
/// with the equations. However, if "a == b" *is* consistent, then "a != b" MUST be false.
/// This is close, but not always the case.
///
/// Consider the equations "a = 1", "b != 1". Using the same method above, if we assert "b != 1", then logically "b == 1"
/// should not be consistent. However, "b == 1" is still consistent, yet "b != 1" is not necessarily inconsistent.
/// To illustrate this point further, here is another example that goes beyond simple disjoint equations.
/// a = 1
/// b = 2
/// 17 - c != a + b
///
/// The initial system is
/// [ 1 0 | 1 ]
/// [ 0 1 | 2 ]
/// Now, is "17 - c != a + b" inconsistent with this? Here is the simplified matrix
/// [ 1 0 0 | 1  ]    [ 1 0 0 | 1  ]
/// [ 0 1 0 | 2  ] -> [ 0 1 0 | 2  ]
/// [ 1 1 1 | 17 ]    [ 0 0 1 | 14 ]
/// It is clear that our negated equation is consistent! But that doesn't mean that "17 - c == a + b" is true.
/// The variable `c` acts as a 'scapegoat' since we knew nothing about it until this point.
///
/// My revised solution is to discard any negated equations adding new variables to the system since their addition is trivially consistent,
/// yet says nothing about their truth.
///
/// As a closing thought, I think this problem is related to implicit quantifiers on the variables of equations.
/// Equations imply the universal quantifier and negated equations imply the existential quantifier.
/// This is probably due to how negating quantifiers works: ¬∀x:f(x) = ∃x:¬f(x)
/// Hopefully, this lays the groundwork to attack the problem of quantifiers when I get to it.
///
/// Dec 15, 2024: Third time's the charm!
///
/// Wow! Didn't take long to find a counter example for this one. Consider the following equations.
/// a = b
/// b != 1
///
/// Using the current method, this would say that b != 1 is inconsistent with the system which it isn't.
///
/// Its all about rank! If we add some number (>0) of equations to the system and the rank increases *with no inconsistencies* (0 != 1)
/// then we found "valid solutions" for however many variables the rank increased by.
/// Essentially, we've reduced the set of solutions (assuming we aren't adding any new variables to it),
/// which means that there still exists a solution where all the regular equations and negated equations agree.
///
/// Wait, sets? Yes! I believe thinking about this problem as sets is the most revealing because it doesn't have all these funny exceptions.
/// Each equation on its own represents a set of solutions.
/// Taking the intersection of however many of these "equation sets" (as I'll be referring to them), gives the solution to a system of equations.
/// Taking the complement of an equation set gives the equation set for the negated equation.
///
/// After some thinking, I came up with this equation which is generally true for any sets.
/// A - (A ∩ C(B)) = A ∩ B
/// If A ∩ B = ∅ then we know the equations describing A and B must be inconsistent.
/// Additionally, A ∩ B = ∅ <-> A = A ∩ C(B).
/// This gives us a method to verify if a negated equation is consistent with a system.
/// So lets look at the same problem again in the context of sets
///
/// A: x = y
/// B: y != 1
///
/// A ∩ C(B)'s matrix:
/// [ 1 -1  0 ] -> [ 1 0 1 ]
/// [ 0  1  1 ]    [ 0 1 1 ]
///
/// A's matrix
/// [ 1 -1 0 ]
///
/// Clearly, A != A ∩ C(B), and therefore A ∩ B != ∅, which means A and B are consistent!
///
/// To tie this back to my thought from the previous update, by adding a new variable, you will *never* be able to get A = A ∩ C(B).
///
/// So here we are!! I feel even more confident in this solution than the previous two. Here's to this being it!

/// Represents some variable and a coefficient
#[derive(Debug, Clone, Default)]
pub struct Term<'a> {
    pub coeff: f64,
    pub var: &'a str,
}

/// Represents a sum of terms
pub type LinearExpression<'a> = Vec<Term<'a>>;

/// An equation is something of the form "x = as + bt + ... + cu"
/// where a, b, c are constants and s, t, u are variables
#[derive(Debug, Clone)]
pub struct Equation<'a> {
    pub lhs: LinearExpression<'a>,
    pub rhs: LinearExpression<'a>,
}

#[derive(Debug, Clone)]
pub struct LinearSystem<'a> {
    vars: Vec<&'a str>,
    vars_to_index: HashMap<&'a str, usize>,
    matrix: RefCell<DMatrix<f64>>,
}

impl<'a> LinearSystem<'a> {
    pub fn from_equations(eqns: &'a [&'a Equation]) -> Self {
        let mut vars_to_index: HashMap<&str, usize> = HashMap::new();
        let mut vars: Vec<&str> = Vec::new();
        let mut rows: Vec<Vec<f64>> = Vec::new();

        // Turn equations into matrix
        for eqn in eqns {
            let eqn = eqn.to_equals_zero();
            let mut row: Vec<f64> = Vec::new();
            for term in eqn {
                let pos = if let Some(pos) = vars_to_index.get(term.var) {
                    *pos
                } else {
                    let i = vars.len();
                    vars.push(term.var);
                    vars_to_index.insert(term.var, i);
                    i
                };
                if pos >= row.len() {
                    row.resize(pos + 1, 0.0);
                }
                row[pos] = term.coeff as f64;
            }
            rows.push(row);
        }

        let matrix = DMatrix::from_row_iterator(
            rows.len(),
            vars.len(),
            rows.into_iter()
                .map(|mut x| {
                    x.resize(vars.len(), 0.0);
                    x
                })
                .flatten(),
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

        let Some(ci) = self.vars_to_index.get("") else {
            return true;
        };
        self.matrix
            .borrow()
            .row_iter()
            .filter(|x| x[*ci] != 0.0)
            .find(|x| {
                x.iter()
                    .enumerate()
                    .find(|(i, x)| i != ci && **x != 0.0)
                    .is_none()
            })
            .is_none()
    }

    pub fn add_equation_limited(&self, eqn: &Equation) -> anyhow::Result<()> {
        let eqn = eqn.to_equals_zero();
        let mut row: Vec<f64> = vec![0.0; self.vars.len()];
        for term in eqn {
            let Some(pos) = self.vars_to_index.get(term.var) else {
                bail!("can't insert extra var")
            };
            row[*pos] = term.coeff as f64;
        }
        self.matrix.replace_with(|m| {
            let mut m = m.clone().insert_row(m.nrows(), 0.0);
            m.row_mut(m.nrows() - 1).copy_from_slice(&row);
            m
        });
        Ok(())
    }
}

impl<'a> Display for LinearSystem<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for var in self.vars.iter() {
            f.write_str(*var)?;
            f.write_str(" ")?;
        }
        Display::fmt(&self.matrix.borrow(), f)
    }
}

// NOTE: This doesn't account for transposed columns where the coresponding variables' order is different.
//       This saves on performance
impl<'a> PartialEq for LinearSystem<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.vars == other.vars
            && self
                .matrix
                .borrow()
                .row_iter()
                .zip_longest(other.matrix.borrow().row_iter())
                .find(|x| {
                    if let EitherOrBoth::Both(a, b) = x {
                        a != b
                    } else {
                        false
                    }
                })
                .is_none()
    }
}

impl<'a> Term<'a> {
    pub fn new(var: &'a str, coeff: f64) -> Self {
        Self { var, coeff }
    }
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
    pub fn to_equals_zero(&self) -> LinearExpression<'a> {
        let c = self.clone();

        // term_side is where the terms are going, zero_side is where no terms should be
        let (mut term_side, zero_side) = if self.lhs.len() > self.rhs.len() {
            (c.lhs, c.rhs)
        } else {
            (c.rhs, c.lhs)
        };
        let mut new_terms = zero_side.into_iter().map(|x| -x).collect();
        term_side.append(&mut new_terms);
        term_side
    }

    pub fn has_extra_vars(&self, eqns: &Vec<&Equation>) -> bool {
        let mut set = HashSet::new();
        for eqn in eqns {
            for t in &eqn.lhs {
                set.insert(t.var);
            }
            for t in &eqn.rhs {
                set.insert(t.var);
            }
        }
        for t in &self.lhs {
            if !set.contains(t.var) {
                return true;
            }
        }
        for t in &self.rhs {
            if !set.contains(t.var) {
                return true;
            }
        }
        false
    }
}

fn main() {
    let eqn1 = Equation {
        lhs: vec![Term::new("a", 1.0), Term::new("c", 1.0)],
        rhs: vec![Term::new("", 1.0)],
    };
    let eqn2 = Equation {
        lhs: vec![Term::new("b", 1.0), Term::new("c", 1.0)],
        rhs: vec![Term::new("", 1.0)],
    };
    let eqn3 = Equation {
        lhs: vec![Term::new("a", 1.0)],
        rhs: vec![Term::new("b", 1.0)],
    };
    let eqn4 = Equation {
        lhs: vec![Term::new("a", 1.0)],
        rhs: vec![Term::new("b", 1.0)],
    };

    for a in 0..=1 {
        for b in 0..=1 {
            for c in 0..=1 {
                //for d in 0..=1 {
                let mut eqns = Vec::new();
                let mut ngtd_eqns = Vec::new();

                if a == 1 {
                    eqns.push(&eqn1);
                } else {
                    ngtd_eqns.push(&eqn1);
                }
                if b == 1 {
                    eqns.push(&eqn2);
                } else {
                    ngtd_eqns.push(&eqn2);
                }
                if c == 1 {
                    eqns.push(&eqn3);
                } else {
                    ngtd_eqns.push(&eqn3);
                }
                // if d == 1 {
                //     eqns.push(&eqn4);
                // } else {
                //     ngtd_eqns.push(&eqn4);
                // }

                let system = LinearSystem::from_equations(&eqns);
                let mut consistent = system.is_consistent();

                if consistent {
                    for eqn in ngtd_eqns {
                        let new_system = system.clone();
                        if new_system.add_equation_limited(eqn).is_err() {
                            continue;
                        }
                        new_system.calc_rref();

                        if system == new_system {
                            consistent = false;
                            break;
                        }
                    }
                }
                println!("{a}{b}{c} {consistent}");
                //}
            }
        }
    }
}
