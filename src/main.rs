#[derive(Debug)]
enum Token {
    And,
    Or,
    Conditional,
    Biconditional,
    Not,
    LeftParen,
    RightParen,
    Predicate(String),
}

fn tokenize(input: String) -> Vec<Token> {
    input
        .split(" ")
        .map(|x| match x.trim() {
            "(" => Token::LeftParen,
            ")" => Token::RightParen,
            "&" => Token::And,
            "|" => Token::Or,
            "!" => Token::Not,
            "->" => Token::Conditional,
            "<->" => Token::Biconditional,
            d => Token::Predicate(d.to_string()),
        })
        .collect()
}

fn main() {
    let tokens = tokenize("( A & B ) -> ( B & A )".to_string());
    println!("{tokens:?}");
}
