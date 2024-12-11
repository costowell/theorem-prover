use std::{collections::HashMap, fmt::Display, iter::Peekable, slice::Iter};

use anyhow::{anyhow, bail};

use crate::Token;

pub struct Parser<'a> {
    pub iter: &'a mut Peekable<Iter<'a, Token>>,
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

#[derive(Debug)]
pub enum Expression {
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Predicate(String),
}

impl Expression {
    pub fn eval(&self, map: &HashMap<&str, bool>) -> bool {
        match self {
            Self::Binary(BinaryOperator::Conditional, e1, e2) => !e1.eval(map) || e2.eval(map),
            Self::Binary(BinaryOperator::Biconditional, e1, e2) => e1.eval(map) == e2.eval(map),
            Self::Binary(BinaryOperator::And, e1, e2) => e1.eval(map) && e2.eval(map),
            Self::Binary(BinaryOperator::Or, e1, e2) => e1.eval(map) || e2.eval(map),
            Self::Unary(UnaryOperator::Not, e1) => !e1.eval(map),
            Self::Predicate(p) => *map.get(p.as_str()).unwrap(),
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
            Self::Predicate(s) => f.write_str(s.as_str()),
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
    pub fn new(iter: &'a mut Peekable<Iter<'a, Token>>) -> Self {
        Self { iter }
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

    fn peek(&mut self) -> anyhow::Result<&&Token> {
        self.iter.peek().ok_or(anyhow!("Unexpected end of input"))
    }

    fn assert_next(&mut self, token: Token) -> anyhow::Result<()> {
        let next = self.next()?;
        if *next != token {
            bail!("Expected {:?}, got {:?}", token, next);
        }
        Ok(())
    }

    fn substatement(&mut self) -> anyhow::Result<Expression> {
        if **self.peek()? == Token::LeftParen {
            self.next()?;
            let stmt1 = self.statement()?;
            let op = BinaryOperator::try_from(self.next()?)?;
            let stmt2 = self.statement()?;
            self.assert_next(Token::RightParen)?;
            Ok(Expression::Binary(op, Box::new(stmt1), Box::new(stmt2)))
        } else {
            self.predicate()
        }
    }

    fn statement(&mut self) -> anyhow::Result<Expression> {
        if **self.peek()? == Token::Not {
            self.next()?;
            let stmt1 = self.substatement()?;
            Ok(Expression::Unary(UnaryOperator::Not, Box::new(stmt1)))
        } else {
            self.substatement()
        }
    }

    fn predicate(&mut self) -> anyhow::Result<Expression> {
        let next = self.next()?;
        let Token::Predicate(p) = next else {
            bail!("Expected Predicate, got {:?}", next);
        };
        Ok(Expression::Predicate(p.clone()))
    }
}
