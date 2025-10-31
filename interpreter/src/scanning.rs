pub fn get_tokens(program: &str) -> Result<Vec<Token>, ScanningError> {
    let mut tokens = vec![];
    let mut line = 1;
    let mut curr = 0;
    let mut has_error = false;
    let chars: Vec<char> = program.chars().collect();

    while curr < chars.len() {
        let next_ch = chars.get(curr + 1);
        let token = match chars[curr] {
            '(' => Some(TokenType::LeftParen),
            ')' => Some(TokenType::RightParen),
            '{' => Some(TokenType::LeftBrace),
            '}' => Some(TokenType::RightBrace),
            ',' => Some(TokenType::Comma),
            '.' => Some(TokenType::Dot),
            '-' => Some(TokenType::Minus),
            '+' => Some(TokenType::Plus),
            ';' => Some(TokenType::Semicolon),
            '*' => Some(TokenType::Star),
            '/' if next_ch == Some(&'/') => {
                // '//' comments: scan to end of line or EOF and discard chars (no token)
                while curr < chars.len() && chars[curr] != '\n' {
                    curr += 1;
                }
                None
            }
            '/' => Some(TokenType::Slash),
            ch @ ('!' | '=' | '<' | '>') if next_ch == Some(&'=') => {
                curr += 1; // consume the equals character because it's part of the current token
                Some(match ch {
                    '!' => TokenType::BangEqual,
                    '=' => TokenType::EqualEqual,
                    '<' => TokenType::LessEqual,
                    '>' => TokenType::GreaterEqual,
                    _ => unreachable!(),
                })
            }
            '!' => Some(TokenType::Bang),
            '=' => Some(TokenType::Equal),
            '<' => Some(TokenType::Less),
            '>' => Some(TokenType::Greater),
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
                    Some(TokenType::String(string))
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

                let number = TokenType::Number(
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
                    "and" => TokenType::And,
                    "class" => TokenType::Class,
                    "else" => TokenType::Else,
                    "false" => TokenType::False,
                    "for" => TokenType::For,
                    "fun" => TokenType::Fun,
                    "if" => TokenType::If,
                    "nil" => TokenType::Nil,
                    "or" => TokenType::Or,
                    "print" => TokenType::Print,
                    "return" => TokenType::Return,
                    "super" => TokenType::Super,
                    "this" => TokenType::This,
                    "true" => TokenType::True,
                    "var" => TokenType::Var,
                    "while" => TokenType::While,
                    _ => TokenType::Identifier(text),
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

        if let Some(token_type) = token {
            tokens.push(Token { line, token_type });
        }

        curr += 1;
    }

    tokens.push(Token {
        token_type: TokenType::EOF,
        line,
    });

    println!("tokens: {:?}", tokens);

    if has_error {
        Err(ScanningError::Error)
    } else {
        Ok(tokens)
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub line: usize,
    pub token_type: TokenType,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
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
        let tokens: Vec<_> = get_tokens(source)
            .unwrap()
            .into_iter()
            .map(|t| t.token_type)
            .collect();
        assert_eq!(tokens.len(), 16);
        assert_eq!(tokens[0], TokenType::LeftParen);
        assert_eq!(tokens[1], TokenType::RightParen);
        assert_eq!(tokens[2], TokenType::LeftBrace);
        assert_eq!(tokens[3], TokenType::RightBrace);
        assert_eq!(tokens[4], TokenType::Comma);
        assert_eq!(tokens[5], TokenType::Dot);
        assert_eq!(tokens[6], TokenType::Minus);
        assert_eq!(tokens[7], TokenType::Plus);
        assert_eq!(tokens[8], TokenType::Semicolon);
        assert_eq!(tokens[9], TokenType::Slash);
        assert_eq!(tokens[10], TokenType::Star);
        assert_eq!(tokens[11], TokenType::Bang);
        assert_eq!(tokens[12], TokenType::Equal);
        assert_eq!(tokens[13], TokenType::Less);
        assert_eq!(tokens[14], TokenType::Greater);
        assert_eq!(tokens[15], TokenType::EOF);
    }

    #[test]
    fn two_char_tokens() {
        let source = "!= == <= >=";
        let tokens = get_token_types(source);
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], TokenType::BangEqual);
        assert_eq!(tokens[1], TokenType::EqualEqual);
        assert_eq!(tokens[2], TokenType::LessEqual);
        assert_eq!(tokens[3], TokenType::GreaterEqual);
        assert_eq!(tokens[4], TokenType::EOF);
    }

    #[test]
    fn string_token() {
        let source = "\"hello world\"";
        let tokens = get_token_types(source);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], TokenType::String("hello world".to_string()));
        assert_eq!(tokens[1], TokenType::EOF);
    }

    #[test]
    fn trivia_is_ignored() {
        let source = "   \n\t // This is a comment\n";
        let tokens = get_token_types(source);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], TokenType::EOF);
    }

    #[test]
    fn numbers_without_decimal_point() {
        let source = "1234";
        let tokens = get_token_types(source);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], TokenType::Number(1234.0));
        assert_eq!(tokens[1], TokenType::EOF);
    }

    #[test]
    fn numbers_with_decimal_point() {
        let source = "123.4";
        let tokens = get_token_types(source);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], TokenType::Number(123.4));
        assert_eq!(tokens[1], TokenType::EOF);
    }

    #[test]
    fn keywords() {
        let source =
            "and class else false for fun if nil or print return super this true var while";
        let tokens = get_token_types(source);
        assert_eq!(tokens.len(), 17);
        assert_eq!(tokens[0], TokenType::And);
        assert_eq!(tokens[1], TokenType::Class);
        assert_eq!(tokens[2], TokenType::Else);
        assert_eq!(tokens[3], TokenType::False);
        assert_eq!(tokens[4], TokenType::For);
        assert_eq!(tokens[5], TokenType::Fun);
        assert_eq!(tokens[6], TokenType::If);
        assert_eq!(tokens[7], TokenType::Nil);
        assert_eq!(tokens[8], TokenType::Or);
        assert_eq!(tokens[9], TokenType::Print);
        assert_eq!(tokens[10], TokenType::Return);
        assert_eq!(tokens[11], TokenType::Super);
        assert_eq!(tokens[12], TokenType::This);
        assert_eq!(tokens[13], TokenType::True);
        assert_eq!(tokens[14], TokenType::Var);
        assert_eq!(tokens[15], TokenType::While);
        assert_eq!(tokens[16], TokenType::EOF);
    }

    #[test]
    fn identifier() {
        let source = "myVariable foo123 bar";
        let tokens = get_token_types(source);
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], TokenType::Identifier("myVariable".to_string()));
        assert_eq!(tokens[1], TokenType::Identifier("foo123".to_string()));
        assert_eq!(tokens[2], TokenType::Identifier("bar".to_string()));
        assert_eq!(tokens[3], TokenType::EOF);
    }

    fn get_token_types(program: &str) -> Vec<TokenType> {
        get_tokens(program)
            .unwrap()
            .into_iter()
            .map(|token| token.token_type)
            .collect()
    }
}
