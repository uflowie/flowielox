pub fn get_tokens(program: &str) -> Result<Vec<Token>, ScanningError> {
    let mut tokens = vec![];
    let mut line = 1;
    let mut curr = 0;
    let mut has_error = false;
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
                curr += 1;
                let string_start = curr;

                while curr < chars.len() && chars[curr] != '"' {
                    curr += 1;
                }

                if curr >= chars.len() || chars[curr] != '"' {
                    has_error = true;
                    println!("error on line {}, unterminated string", line);
                    None
                } else {
                    let string = chars[string_start..curr].iter().collect();
                    Some(Token::String(string))
                }
            }
            ch if ch.is_ascii_digit() => {
                let start = curr;

                while curr < chars.len() && chars[curr].is_ascii_digit() {
                    curr += 1;
                }

                if chars.get(curr) == Some(&'.')
                    && chars.get(curr + 1).is_some_and(|c| c.is_ascii_digit())
                {
                    curr += 1;
                    while curr < chars.len() && chars[curr].is_ascii_digit() {
                        curr += 1;
                    }
                }

                let number = Token::Number(
                    chars[start..curr]
                        .iter()
                        .collect::<String>()
                        .parse::<f64>()
                        .unwrap(),
                );
                curr -= 1;

                Some(number)
            }
            ch if ch.is_alphanumeric() => {
                let start = curr;
                while curr < chars.len() && chars[curr].is_alphanumeric() {
                    curr += 1;
                }

                let text = chars[start..curr].iter().collect::<String>();
                let token = match text.as_str() {
                    "and" => Token::And,
                    "class" => Token::Class,
                    "else" => Token::Else,
                    "false" => Token::False,
                    "for" => Token::For,
                    "fun" => Token::Fun,
                    "if" => Token::If,
                    "nil" => Token::Nil,
                    "or" => Token::Or,
                    "print" => Token::Print,
                    "return" => Token::Return,
                    "super" => Token::Super,
                    "this" => Token::This,
                    "true" => Token::True,
                    "var" => Token::Var,
                    "while" => Token::While,
                    _ => Token::Identifier(text),
                };

                curr -= 1;
                Some(token)
            }
            _ => {
                println!("error on line {}", line);
                has_error = true;
                None
            }
        };

        if let Some(t) = token {
            tokens.push(t);
        }

        curr += 1;
    }

    tokens.push(Token::EOF);

    println!("tokens: {:?}", tokens);

    if has_error {
        Err(ScanningError::Error)
    } else {
        Ok(tokens)
    }
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
    Number(f64),
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
pub enum ScanningError {
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_char_tokens() {
        let source = "(){},.-+;/*! = < >"; // (spaces to make ! = < > parse as single char tokens)
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens.len(), 16);
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
    fn two_char_tokens() {
        let source = "!= == <= >=";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], Token::BangEqual);
        assert_eq!(tokens[1], Token::EqualEqual);
        assert_eq!(tokens[2], Token::LessEqual);
        assert_eq!(tokens[3], Token::GreaterEqual);
        assert_eq!(tokens[4], Token::EOF);
    }

    #[test]
    fn string_token() {
        let source = "\"hello world\"";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::String("hello world".to_string()));
        assert_eq!(tokens[1], Token::EOF);
    }

    #[test]
    fn trivia_is_ignored() {
        let source = "   \n\t // This is a comment\n";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], Token::EOF);
    }

    #[test]
    fn numbers_without_decimal_point() {
        let source = "1234";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::Number(1234.0));
        assert_eq!(tokens[1], Token::EOF);
    }

    #[test]
    fn numbers_with_decimal_point() {
        let source = "123.4";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], Token::Number(123.4));
        assert_eq!(tokens[1], Token::EOF);
    }

    #[test]
    fn keywords() {
        let source =
            "and class else false for fun if nil or print return super this true var while";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens.len(), 17);
        assert_eq!(tokens[0], Token::And);
        assert_eq!(tokens[1], Token::Class);
        assert_eq!(tokens[2], Token::Else);
        assert_eq!(tokens[3], Token::False);
        assert_eq!(tokens[4], Token::For);
        assert_eq!(tokens[5], Token::Fun);
        assert_eq!(tokens[6], Token::If);
        assert_eq!(tokens[7], Token::Nil);
        assert_eq!(tokens[8], Token::Or);
        assert_eq!(tokens[9], Token::Print);
        assert_eq!(tokens[10], Token::Return);
        assert_eq!(tokens[11], Token::Super);
        assert_eq!(tokens[12], Token::This);
        assert_eq!(tokens[13], Token::True);
        assert_eq!(tokens[14], Token::Var);
        assert_eq!(tokens[15], Token::While);
        assert_eq!(tokens[16], Token::EOF);
    }

    #[test]
    fn identifier() {
        let source = "myVariable foo123 bar";
        let tokens = get_tokens(source).unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], Token::Identifier("myVariable".to_string()));
        assert_eq!(tokens[1], Token::Identifier("foo123".to_string()));
        assert_eq!(tokens[2], Token::Identifier("bar".to_string()));
        assert_eq!(tokens[3], Token::EOF);
    }
}
