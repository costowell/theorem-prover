use anyhow::bail;
use regex::Regex;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Conditional,
    Biconditional,
    And,
    Or,
    Not,
    Add,
    Sub,
    Equals,
    LeftParen,
    RightParen,
    Number(f64),
    Identifier(String),
}

pub fn tokenize(input: String) -> anyhow::Result<Vec<Token>> {
    let re = Regex::new(
        r"([A-z]+)|(-?(?:[0-9]*[.])?[0-9]+)|(\()|(\))|(!)|(&)|(\|)|(->)|(<->)|(=)|(\+)|(-)",
    )
    .unwrap();
    let mut result = vec![];
    for t in re.find_iter(input.as_str()).map(|x| x.as_str()) {
        let token = match t {
            "&" => Some(Token::And),
            "|" => Some(Token::Or),
            "!" => Some(Token::Not),
            "(" => Some(Token::LeftParen),
            ")" => Some(Token::RightParen),
            "->" => Some(Token::Conditional),
            "<->" => Some(Token::Biconditional),
            "+" => Some(Token::Add),
            "-" => Some(Token::Sub),
            "=" => Some(Token::Equals),
            _ => None,
        };
        if let Some(token) = token {
            result.push(token);
        } else if t.chars().next().unwrap().is_ascii_alphabetic() {
            result.push(Token::Identifier(t.to_string()))
        } else if t.chars().next().unwrap().is_numeric() {
            result.push(Token::Number(t.parse::<f64>()?))
        } else {
            bail!("Unexpected token, {}", t)
        }
    }
    Ok(result)
}
