use anyhow::bail;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Conditional,
    Biconditional,
    And,
    Or,
    Not,
    LeftParen,
    RightParen,
    Predicate(String),
}

pub fn tokenize(input: String) -> anyhow::Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(next) = chars.next() {
        let token = match next {
            '&' => Token::And,
            '|' => Token::Or,
            '!' => Token::Not,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '-' => {
                if chars.next() == Some('>') {
                    Token::Conditional
                } else {
                    bail!("failed to parse tokens, expected '->'");
                }
            }
            '<' => {
                if chars.next() == Some('-') && chars.next() == Some('>') {
                    Token::Biconditional
                } else {
                    bail!("failed to parse tokens, expected '<->'");
                }
            }
            'A'..='Z' | 'a'..='z' => {
                let mut str = String::new();
                str.push(next);
                while matches!(chars.peek(), Some('A'..='Z' | 'a'..='z')) {
                    str.push(chars.next().unwrap());
                }
                Token::Predicate(str)
            }
            ' ' => continue,
            _ => bail!("failed to parse tokens, unexpected char '{}'", next),
        };
        tokens.push(token);
    }
    Ok(tokens)
}
