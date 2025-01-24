use std::{fmt::Display, iter::Peekable, ops::Neg, slice::Iter};

use anyhow::{anyhow, bail};

use crate::lexer::Token;

pub struct Parser<'a> {
    pub iter: Peekable<Iter<'a, Token>>,
    pub predicate_count: u32,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Conditional,
    Biconditional,
    And,
    Or,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Not,
}

/// If the `var_name` is None, it is a constant
#[derive(Debug, Clone)]
pub struct Term {
    pub name: Option<String>,
    pub coeff: f64,
}

pub type LinearExpression = Vec<Term>;

/// LHS and RHS represent a vector of summed terms
#[derive(Debug, Clone)]
pub struct Equation {
    pub lhs: LinearExpression,
    pub rhs: LinearExpression,
}

#[derive(Debug)]
pub enum PredicateValue {
    Name(String),
    Equation(Equation),
}

#[derive(Debug)]
pub struct Predicate {
    pub ident: u32,
    pub value: PredicateValue,
}

#[derive(Debug)]
pub enum Expression {
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Predicate(Predicate),
}

impl Term {
    pub fn new(name: Option<String>, coeff: f64) -> Self {
        Self { name, coeff }
    }
}

impl Neg for Term {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.coeff *= -1.0;
        self
    }
}

impl Equation {
    pub fn new(lhs: LinearExpression, rhs: LinearExpression) -> Self {
        Self { lhs, rhs }
    }
    pub fn to_equals_zero(&self) -> LinearExpression {
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
}

impl Predicate {
    pub fn new(ident: u32, value: PredicateValue) -> Self {
        Self { ident, value }
    }
}

impl Expression {
    pub fn eval(&self, map: &[bool]) -> bool {
        match self {
            Self::Binary(BinaryOperator::Conditional, e1, e2) => !e1.eval(map) || e2.eval(map),
            Self::Binary(BinaryOperator::Biconditional, e1, e2) => e1.eval(map) == e2.eval(map),
            Self::Binary(BinaryOperator::And, e1, e2) => e1.eval(map) && e2.eval(map),
            Self::Binary(BinaryOperator::Or, e1, e2) => e1.eval(map) || e2.eval(map),
            Self::Unary(UnaryOperator::Not, e1) => !e1.eval(map),
            Self::Predicate(p) => *map.get(p.ident as usize).unwrap(),
        }
    }
    pub fn get_predicates(&self) -> Vec<&Predicate> {
        match self {
            Self::Binary(_, e1, e2) => {
                let mut p = e1.get_predicates();
                p.append(&mut e2.get_predicates());
                p
            }
            Self::Unary(_, e1) => e1.get_predicates(),
            Self::Predicate(p) => vec![p],
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Biconditional => "<->",
            Self::Conditional => "->",
            Self::Or => "|",
            Self::And => "&",
        })
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Not => "!",
        })
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.coeff.to_string().as_str())?;
        if let Some(name) = &self.name {
            f.write_str(name)?;
        }
        Ok(())
    }
}

impl Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.lhs.iter();
        iter.next().unwrap().fmt(f)?;
        for item in iter {
            f.write_str(" + ")?;
            item.fmt(f)?;
        }
        f.write_str(" = ")?;

        let mut iter = self.rhs.iter();
        iter.next().unwrap().fmt(f)?;
        for item in iter {
            f.write_str(" + ")?;
            item.fmt(f)?;
        }
        Ok(())
    }
}

impl Display for PredicateValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name(name) => f.write_str(name.as_str()),
            Self::Equation(eq) => {
                f.write_str("(")?;
                eq.fmt(f)?;
                f.write_str(")")
            }
        }
    }
}

impl Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(op, e1, e2) => {
                f.write_str("(")?;
                e1.fmt(f)?;
                op.fmt(f)?;
                e2.fmt(f)?;
                f.write_str(")")
            }
            Self::Unary(op, e1) => {
                op.fmt(f)?;
                e1.fmt(f)
            }
            Self::Predicate(s) => s.fmt(f),
        }
    }
}

impl TryFrom<&Token> for BinaryOperator {
    type Error = anyhow::Error;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Conditional => Ok(Self::Conditional),
            Token::Biconditional => Ok(Self::Biconditional),
            Token::And => Ok(Self::And),
            Token::Or => Ok(Self::Or),
            _ => Err(anyhow!("Not a binary operator")),
        }
    }
}

impl TryFrom<&Token> for UnaryOperator {
    type Error = anyhow::Error;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value {
            Token::Not => Ok(Self::Not),
            _ => Err(anyhow!("Not a unary operator")),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(iter: Peekable<Iter<'a, Token>>) -> Self {
        Self {
            iter,
            predicate_count: 0,
        }
    }

    pub fn parse(&mut self) -> anyhow::Result<Expression> {
        let stmt1 = self.statement()?;
        if let Ok(next) = self.next() {
            let op = BinaryOperator::try_from(next)?;
            let stmt2 = self.statement()?;
            return Ok(Expression::Binary(op, Box::new(stmt1), Box::new(stmt2)));
        }
        Ok(stmt1)
    }

    fn next(&mut self) -> anyhow::Result<&Token> {
        self.iter.next().ok_or(anyhow!("Unexpected end of input"))
    }

    fn peek(&mut self) -> Option<&&Token> {
        self.iter.peek()
    }

    fn assert_next(&mut self, token: Token) -> anyhow::Result<()> {
        let next = self.next()?;
        if *next != token {
            bail!("Expected {:?}, got {:?}", token, next);
        }
        Ok(())
    }

    fn term(&mut self) -> anyhow::Result<Term> {
        let next = self.next()?.clone();
        match next {
            Token::Number(num) => {
                if let Some(Token::Identifier(var)) = self.peek() {
                    let var = var.clone();
                    self.next()?;
                    Ok(Term::new(Some(var), num))
                } else {
                    Ok(Term::new(None, num))
                }
            }
            Token::Identifier(var) => Ok(Term::new(Some(var.clone()), 1.0)),
            _ => bail!("Expected term, got "),
        }
    }

    fn linear_expression(&mut self) -> anyhow::Result<LinearExpression> {
        let mut terms = vec![self.term()?];
        while let Some(peek) = self.peek() {
            let modifier = if matches!(peek, Token::Add) {
                1.0
            } else if matches!(peek, Token::Sub) {
                -1.0
            } else {
                break;
            };
            self.next()?;

            let mut term = self.term()?;
            term.coeff *= modifier;
            terms.push(term);
        }
        Ok(terms)
    }

    fn algebraic_statement(&mut self) -> anyhow::Result<Expression> {
        self.assert_next(Token::LeftParen)?;
        let lhs = self.linear_expression()?;
        self.assert_next(Token::Equals)?;
        let rhs = self.linear_expression()?;
        self.assert_next(Token::RightParen)?;

        let pred = Predicate::new(
            self.predicate_count,
            PredicateValue::Equation(Equation::new(lhs, rhs)),
        );
        self.predicate_count += 1;
        Ok(Expression::Predicate(pred))
    }

    fn boolean_statement(&mut self) -> anyhow::Result<Expression> {
        self.assert_next(Token::LeftParen)?;
        let stmt1 = self.statement()?;
        let op = BinaryOperator::try_from(self.next()?)?;
        let stmt2 = self.statement()?;
        self.assert_next(Token::RightParen)?;
        Ok(Expression::Binary(op, Box::new(stmt1), Box::new(stmt2)))
    }

    fn substatement(&mut self) -> anyhow::Result<Expression> {
        let tmp = (self.iter.clone(), self.predicate_count);
        if let Ok(stmt) = self.boolean_statement() {
            return Ok(stmt);
        }
        self.iter = tmp.0.clone();
        self.predicate_count = tmp.1;

        if let Ok(stmt) = self.algebraic_statement() {
            return Ok(stmt);
        }
        self.iter = tmp.0;
        self.predicate_count = tmp.1;

        self.predicate().map_err(|_| anyhow!("Failed to parse substatement"))
    }

    fn statement(&mut self) -> anyhow::Result<Expression> {
        if matches!(self.peek(), Some(Token::Not)) {
            self.next()?;
            let stmt1 = self.substatement()?;
            Ok(Expression::Unary(UnaryOperator::Not, Box::new(stmt1)))
        } else {
            self.substatement()
        }
    }

    fn predicate(&mut self) -> anyhow::Result<Expression> {
        let next = self.next()?;
        let Token::Identifier(p) = next else {
            bail!("Expected Identifier, got {:?}", next);
        };
        let p = p.clone();
        let pred = Predicate::new(self.predicate_count, PredicateValue::Name(p));
        self.predicate_count += 1;
        Ok(Expression::Predicate(pred))
    }
}
