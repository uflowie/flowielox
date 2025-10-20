use crate::{
    expressions::{BinaryOperator, Expression, Literal, UnaryOperator},
    scanning::Token,
    statements::Statement,
};

pub fn parse(tokens: &[Token]) -> Vec<Statement> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser<'a> {
    tokens: &'a [Token],
    curr: usize,
}

impl Parser<'_> {
    fn new(tokens: &'_ [Token]) -> Parser<'_> {
        Parser { tokens, curr: 0 }
    }

    fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        while self.is_at_end() {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt);
            }
        }

        statements
    }

    fn declaration(&mut self) -> Option<Statement> {
        let statement = if let Some(Token::Var) = self.curr_token() {
            self.advance();
            self.var_declaration()
        } else {
            self.statement()
        };

        match statement {
            Ok(statement) => Some(statement),
            Err(err) => {
                println!("{:?}", err);
                self.synchronize();
                None
            }
        }
    }

    fn var_declaration(&mut self) -> Result<Statement, ParsingError> {
        let name = match self.curr_token() {
            Some(Token::Identifier(n)) => n.clone(),
            _ => return Err(self.unexpected_token("semicolon")),
        };

        self.advance();

        let mut initializer = None;
        if let Some(Token::Equal) = self.curr_token() {
            self.advance();
            initializer = Some(self.expression()?);
        }

        if let Some(Token::Semicolon) = self.curr_token() {
            self.advance();
            Ok(Statement::Variable { name, initializer })
        } else {
            Err(self.unexpected_token("semicolon"))
        }
    }

    fn statement(&mut self) -> Result<Statement, ParsingError> {
        if let Some(Token::Print) = self.curr_token() {
            self.advance();
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;
        if let Some(Token::Semicolon) = self.curr_token() {
            self.advance();
            Ok(Statement::Print(expr))
        } else {
            Err(self.unexpected_token("semicolon"))
        }
    }

    fn expression_statement(&mut self) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;
        if let Some(Token::Semicolon) = self.curr_token() {
            self.advance();
            Ok(Statement::Expression(expr))
        } else {
            Err(self.unexpected_token("semicolon"))
        }
    }

    fn expression(&mut self) -> Result<Expression, ParsingError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, ParsingError> {
        let mut expression = self.comparison()?;

        while let Some(operator @ (Token::BangEqual | Token::EqualEqual)) = self.curr_token() {
            let operator = match operator {
                Token::BangEqual => BinaryOperator::BangEqual,
                _ => BinaryOperator::EqualEqual,
            };

            self.advance();

            let right = self.comparison()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }

        Ok(expression)
    }

    fn comparison(&mut self) -> Result<Expression, ParsingError> {
        let mut expression = self.term()?;

        while let Some(
            operator @ (Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual),
        ) = self.curr_token()
        {
            let operator = match operator {
                Token::Greater => BinaryOperator::Greater,
                Token::GreaterEqual => BinaryOperator::GreaterEqual,
                Token::Less => BinaryOperator::Less,
                _ => BinaryOperator::LessEqual,
            };

            self.advance();

            let right = self.term()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }

        Ok(expression)
    }

    fn term(&mut self) -> Result<Expression, ParsingError> {
        let mut expression = self.factor()?;

        while let Some(operator @ (Token::Minus | Token::Plus)) = self.curr_token() {
            let operator = match operator {
                Token::Minus => BinaryOperator::Minus,
                _ => BinaryOperator::Plus,
            };

            self.advance();

            let right = self.factor()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }

        Ok(expression)
    }

    fn factor(&mut self) -> Result<Expression, ParsingError> {
        let mut expression = self.unary()?;

        while let Some(operator @ (Token::Slash | Token::Star)) = self.curr_token() {
            let operator = match operator {
                Token::Slash => BinaryOperator::Slash,
                _ => BinaryOperator::Star,
            };

            self.advance();

            let right = self.factor()?;
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }

        Ok(expression)
    }

    fn unary(&mut self) -> Result<Expression, ParsingError> {
        if let Some(operator @ (Token::Bang | Token::Minus)) = self.curr_token() {
            let operator = match operator {
                Token::Bang => UnaryOperator::Bang,
                _ => UnaryOperator::Minus,
            };
            self.advance();
            let right = self.unary()?;
            Ok(Expression::Unary(operator, Box::new(right)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expression, ParsingError> {
        if let Some(
            token @ (Token::True | Token::False | Token::Nil | Token::Number(_) | Token::String(_)),
        ) = self.curr_token()
        {
            let literal = match token {
                Token::False => Literal::Boolean(false),
                Token::True => Literal::Boolean(true),
                Token::Nil => Literal::Nil,
                Token::Number(number) => Literal::Number(*number),
                Token::String(str) => Literal::String(str.clone()),
                _ => unreachable!(),
            };
            self.advance();
            Ok(Expression::Literal(literal))
        } else if let Some(Token::Identifier(name)) = self.curr_token() {
            let name = name.clone();
            self.advance();
            Ok(Expression::Variable(name))
        } else if let Some(Token::LeftParen) = self.curr_token() {
            self.advance();
            let expr = self.expression()?;
            if self.curr_token() != Some(&Token::RightParen) {
                Err(self.unexpected_token("right parenthesis"))
            } else {
                self.advance();
                Ok(Expression::Grouping(Box::new(expr)))
            }
        } else {
            Err(self.unexpected_token("primary token"))
        }
    }

    fn curr_token(&self) -> Option<&Token> {
        self.tokens.get(self.curr)
    }

    fn advance(&mut self) {
        self.curr += 1;
    }

    fn unexpected_token(&self, expected_token: &str) -> ParsingError {
        ParsingError::UnexpectedToken(format!(
            "expected {}, got {:?}",
            expected_token,
            self.curr_token()
        ))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.tokens.get(self.curr - 1) == Some(&Token::Semicolon) {
                return;
            }

            match self.curr_token() {
                Some(
                    Token::Class
                    | Token::Fun
                    | Token::Var
                    | Token::For
                    | Token::If
                    | Token::While
                    | Token::Print
                    | Token::Return,
                ) => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.curr_token() == None || self.curr_token() == Some(&Token::EOF)
    }
}

#[derive(Debug)]
enum ParsingError {
    UnexpectedToken(String),
}
