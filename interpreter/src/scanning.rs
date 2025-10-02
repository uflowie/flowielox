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

        if let Some(t) = token {
            tokens.push(t);
        }

        curr += 1;
    }

    tokens.push(Token::EOF);
    Ok(tokens)
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug)]
pub enum ScanningError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_char_tokens() {
        let source = "(){},.-+;/*! = < >";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens[0], Token::LeftParen);
        assert_eq!(tokens[1], Token::RightParen);
        assert_eq!(tokens[2], Token::LeftBrace);
        assert_eq!(tokens[3], Token::RightBrace);
        assert_eq!(tokens[4], Token::Comma);
        assert_eq!(tokens[5], Token::Dot);
        assert_eq!(tokens[6], Token::Minus);
        assert_eq!(tokens[7], Token::Plus);
        assert_eq!(tokens[8], Token::Semicolon);
        assert_eq!(tokens[9], Token::Slash);
        assert_eq!(tokens[10], Token::Star);
        assert_eq!(tokens[11], Token::Bang);
        assert_eq!(tokens[12], Token::Equal);
        assert_eq!(tokens[13], Token::Less);
        assert_eq!(tokens[14], Token::Greater);
        assert_eq!(tokens[15], Token::EOF);
    }

    #[test]
    fn test_two_char_tokens() {
        let source = "!= == <= >=";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens[0], Token::BangEqual);
        assert_eq!(tokens[1], Token::EqualEqual);
        assert_eq!(tokens[2], Token::LessEqual);
        assert_eq!(tokens[3], Token::GreaterEqual);
        assert_eq!(tokens[4], Token::EOF);
    }
}
