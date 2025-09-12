pub fn get_tokens(program: &str) -> Result<Vec<Token>, ScanningError> {
    let mut tokens = vec![];
    let mut start = 0;
    let mut line = 1;
    let mut curr = 0;
    let chars: Vec<char> = program.chars().collect();

    while curr < chars.len() {
        let next_ch = chars.get(curr + 1);
        let token = match chars[curr] {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            '{' => Some(Token::LeftBrace),
            '}' => Some(Token::RightBrace),
            ',' => Some(Token::Comma),
            '.' => Some(Token::Dot),
            '-' => Some(Token::Minus),
            '+' => Some(Token::Plus),
            ';' => Some(Token::Semicolon),
            '*' => Some(Token::Star),
            '/' if next_ch == Some(&'/') => {
                // '//' comments: scan to end of line or EOF and discard chars (no token)
                while curr < chars.len() && chars[curr] != '\n' {
                    curr += 1;
                }
                None
            }
            '/' => Some(Token::Slash),
            ch @ ('!' | '=' | '<' | '>') if next_ch == Some(&'=') => {
                curr += 1; // consume the equals character because it's part of the current token
                Some(match ch {
                    '!' => Token::BangEqual,
                    '=' => Token::EqualEqual,
                    '<' => Token::LessEqual,
                    '>' => Token::GreaterEqual,
                    _ => unreachable!(),
                })
            }
            '!' => Some(Token::Bang),
            '=' => Some(Token::Equal),
            '<' => Some(Token::Less),
            '>' => Some(Token::Greater),
            ' ' | '\r' | '\t' => None,
            '\n' => {
                line += 1;
                None
            }
            '"' => {
                let string_start = curr + 1;

                while curr < chars.len() && chars[curr] != '"' {
                    curr += 1;
                }

                if chars[curr] == '"' {
                    let string = chars[string_start..curr].iter().collect();
                    Some(Token::String(string))
                } else {
                    println!("error on line {}, unterminated string", line);
                    None
                }
            }
            _ => {
                println!("error on line {}", line);
                None
            }
        };
        curr += 1;
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}

pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(String),
    String(String),
    Number(i64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EOF,
}

pub enum ScanningError {}
