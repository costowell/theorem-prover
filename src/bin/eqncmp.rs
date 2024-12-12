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

/// Variables are just names, nothing special
pub type Variable = String;

/// Represents some variable and a coefficient
pub struct Term {
    pub coeff: i64,
    pub var: Variable,
}

/// Represents a sum of terms
pub type LinearExpression = Vec<Term>;

/// An equation is something of the form "x = as + bt + ... + cu"
/// where a, b, c are constants and s, t, u are variables
pub struct Equation {
    pub lhs: Variable,
    pub rhs: LinearExpression,
}

fn main() {}
