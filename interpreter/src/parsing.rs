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

        while !self.is_at_end() {
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

        self.consume(Token::Semicolon, "semicolon")?;
        Ok(Statement::Variable { name, initializer })
    }

    fn statement(&mut self) -> Result<Statement, ParsingError> {
        match self.curr_token() {
            Some(Token::Print) => {
                self.advance();
                self.print_statement()
            }
            Some(Token::LeftBrace) => {
                self.advance();
                self.block()
            }
            Some(Token::If) => {
                self.advance();
                self.if_statement()
            }
            Some(Token::While) => {
                self.advance();
                self.while_statement()
            }
            Some(Token::For) => {
                self.advance();
                self.for_statement()
            }
            _ => self.expression_statement(),
        }
    }

    fn for_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume(Token::LeftParen, "(")?;

        let initializer = match self.curr_token() {
            Some(Token::Semicolon) => {
                self.advance();
                None
            }
            Some(Token::Var) => {
                self.advance();
                Some(self.var_declaration()?)
            }
            _ => Some(self.expression_statement()?),
        };

        let condition = match self.curr_token() {
            Some(Token::Semicolon) => None,
            _ => Some(self.expression()?),
        };
        self.consume(Token::Semicolon, "semicolon")?;

        let increment = match self.curr_token() {
            Some(Token::RightParen) => None,
            _ => Some(self.expression()?),
        };
        self.consume(Token::RightParen, ")")?;

        let body = self.statement()?;

        let loop_stmt = Statement::While {
            condition: match condition {
                Some(expr) => expr,
                None => Expression::Literal(Literal::Boolean(true)),
            },
            stmt: Box::new(match increment {
                Some(expr) => Statement::Block(vec![body, Statement::Expression(expr)]),
                None => body,
            }),
        };

        Ok(match initializer {
            Some(init) => Statement::Block(vec![init, loop_stmt]),
            None => loop_stmt,
        })
    }

    fn while_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume(Token::LeftParen, "(")?;
        let condition = self.expression()?;
        self.consume(Token::RightParen, ")")?;

        let stmt = Box::new(self.statement()?);

        Ok(Statement::While { condition, stmt })
    }

    fn if_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume(Token::LeftParen, "(")?;
        let condition = self.expression()?;
        self.consume(Token::RightParen, ")")?;

        let then_branch = Box::new(self.statement()?);

        let else_branch = if let Some(Token::Else) = self.curr_token() {
            self.advance();
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Statement::If {
            then_branch,
            condition,
            else_branch,
        })
    }

    fn block(&mut self) -> Result<Statement, ParsingError> {
        let mut statements = Vec::new();

        while !self.is_at_end() && self.curr_token() != Some(&Token::RightBrace) {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt)
            }
        }

        self.consume(Token::RightBrace, "}")?;
        Ok(Statement::Block(statements))
    }

    fn print_statement(&mut self) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;

        self.consume(Token::Semicolon, "semicolon")?;
        Ok(Statement::Print(expr))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;
        self.consume(Token::Semicolon, "semicolon")?;
        Ok(Statement::Expression(expr))
    }

    fn assignment(&mut self) -> Result<Expression, ParsingError> {
        let expr = self.or()?;

        if let Some(Token::Equal) = self.curr_token() {
            self.advance();
            let value = self.assignment()?;

            let Expression::Variable(name) = expr else {
                return Err(ParsingError::IllegalAssignmentTarget);
            };

            Ok(Expression::Assign(name, Box::new(value)))
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.and()?;

        while let Some(Token::Or) = self.curr_token() {
            self.advance();

            let right = self.expression()?;
            expr = Expression::LogicalOr(Box::new(expr), Box::new(right))
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.equality()?;

        while let Some(Token::And) = self.curr_token() {
            self.advance();

            let right = self.expression()?;
            expr = Expression::LogicalAnd(Box::new(expr), Box::new(right))
        }

        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expression, ParsingError> {
        self.assignment()
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

            self.consume(Token::RightParen, ")")?;
            Ok(Expression::Grouping(Box::new(expr)))
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

    fn consume(&mut self, expected_token: Token, error_msg: &str) -> Result<(), ParsingError> {
        if self.curr_token() != Some(&expected_token) {
            Err(ParsingError::UnexpectedToken(error_msg.to_string()))
        } else {
            self.advance();
            Ok(())
        }
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
    IllegalAssignmentTarget,
}
