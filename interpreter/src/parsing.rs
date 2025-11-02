use crate::{
    expressions::{BinaryOperator, Expression, ExpressionType, Literal, UnaryOperator},
    scanning::{Token, TokenType},
    statements::{ClassStatement, FunctionStatement, FunctionType, Statement},
};

pub fn parse(tokens: &[Token]) -> Vec<Statement> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser<'a> {
    tokens: &'a [Token],
    curr: usize,
    next_id: u32,
}

impl Parser<'_> {
    fn new(tokens: &'_ [Token]) -> Parser<'_> {
        Parser {
            tokens,
            curr: 0,
            next_id: 1,
        }
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
        let statement = match self.curr_token_type() {
            Some(TokenType::Var) => {
                self.advance();
                self.var_declaration()
            }
            Some(TokenType::Class) => {
                self.advance();
                self.class_declaration()
            }
            _ => self.statement(),
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
        let name = match self.curr_token_type() {
            Some(TokenType::Identifier(n)) => n.clone(),
            _ => return Err(self.unexpected_token("semicolon")),
        };

        self.advance();

        let mut initializer = None;
        if let Some(TokenType::Equal) = self.curr_token_type() {
            self.advance();
            initializer = Some(self.expression()?);
        }

        self.consume(TokenType::Semicolon, "semicolon")?;
        Ok(Statement::Variable { name, initializer })
    }

    fn class_declaration(&mut self) -> Result<Statement, ParsingError> {
        let Some(TokenType::Identifier(name)) = self.curr_token_type() else {
            return Err(self.unexpected_token("class name"));
        };
        let name = name.clone();
        self.advance();

        let mut superclass = None;

        if self.curr_token_type() == Some(&TokenType::Less) {
            self.advance();

            let Some(Token {
                token_type: TokenType::Identifier(name),
                line,
            }) = self.curr_token()
            else {
                return Err(self.unexpected_token("identifier after superclass"));
            };
            let name = name.clone();
            let line = *line;
            self.advance();
            superclass = Some(self.make_expr_with_line(ExpressionType::Variable(name), line));
        }

        self.consume(TokenType::LeftBrace, "left brace before class body")?;

        let mut methods = vec![];

        while !self.is_at_end() && self.curr_token_type() != Some(&TokenType::RightBrace) {
            methods.push(self.function(FunctionType::Method)?);
        }

        self.consume(TokenType::RightBrace, "right brace after class body")?;

        Ok(Statement::Class(ClassStatement {
            name,
            methods,
            superclass,
        }))
    }

    fn statement(&mut self) -> Result<Statement, ParsingError> {
        match self.curr_token().map(|t| &t.token_type) {
            Some(TokenType::Print) => {
                self.advance();
                self.print_statement()
            }
            Some(TokenType::LeftBrace) => Ok(Statement::Block(self.block()?)),
            Some(TokenType::If) => {
                self.advance();
                self.if_statement()
            }
            Some(TokenType::While) => {
                self.advance();
                self.while_statement()
            }
            Some(TokenType::For) => {
                self.advance();
                self.for_statement()
            }
            Some(TokenType::Fun) => {
                self.advance();
                Ok(Statement::Function(self.function(FunctionType::Function)?))
            }
            Some(TokenType::Return) => {
                self.advance();
                self.return_statement()
            }
            _ => self.expression_statement(),
        }
    }

    fn return_statement(&mut self) -> Result<Statement, ParsingError> {
        if let Some(TokenType::Semicolon) = self.curr_token_type() {
            self.advance();
            Ok(Statement::Return(None))
        } else {
            let expr = self.expression()?;
            self.consume(
                TokenType::Semicolon,
                "expected semicolon after return expression",
            )?;
            Ok(Statement::Return(Some(expr)))
        }
    }

    fn function(&mut self, function_type: FunctionType) -> Result<FunctionStatement, ParsingError> {
        let Some(Token {
            token_type: TokenType::Identifier(name),
            ..
        }) = self.curr_token()
        else {
            return Err(self.unexpected_token("identifier"));
        };
        let name = name.clone();

        self.advance();

        self.consume(TokenType::LeftParen, "(")?;
        let mut params = vec![];

        if self.curr_token_type() != Some(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(ParsingError::TooManyArguments);
                }

                if let Some(TokenType::Identifier(name)) = self.curr_token_type() {
                    params.push(name.clone());
                    self.advance();
                } else {
                    return Err(ParsingError::UnexpectedToken(String::from(
                        "expected identifier",
                    )));
                }

                if self.curr_token_type() == Some(&TokenType::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            "expected ) at the end of function parameter list",
        )?;
        let body = self.block()?;

        Ok(FunctionStatement {
            name,
            params,
            body,
            function_type,
        })
    }

    fn for_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume(TokenType::LeftParen, "(")?;

        let initializer = match self.curr_token_type() {
            Some(TokenType::Semicolon) => {
                self.advance();
                None
            }
            Some(TokenType::Var) => {
                self.advance();
                Some(self.var_declaration()?)
            }
            _ => Some(self.expression_statement()?),
        };

        let condition = match self.curr_token_type() {
            Some(TokenType::Semicolon) => None,
            _ => Some(self.expression()?),
        };
        self.consume(TokenType::Semicolon, "semicolon")?;

        let increment = match self.curr_token_type() {
            Some(TokenType::RightParen) => None,
            _ => Some(self.expression()?),
        };
        self.consume(TokenType::RightParen, ")")?;

        let body = self.statement()?;

        let loop_stmt = Statement::While {
            condition: match condition {
                Some(expr) => expr,
                None => self.make_expr(ExpressionType::Literal(Literal::Boolean(true))),
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
        self.consume(TokenType::LeftParen, "(")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, ")")?;

        let stmt = Box::new(self.statement()?);

        Ok(Statement::While { condition, stmt })
    }

    fn if_statement(&mut self) -> Result<Statement, ParsingError> {
        self.consume(TokenType::LeftParen, "(")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, ")")?;

        let then_branch = Box::new(self.statement()?);

        let else_branch = if let Some(TokenType::Else) = self.curr_token_type() {
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

    fn block(&mut self) -> Result<Vec<Statement>, ParsingError> {
        self.consume(TokenType::LeftBrace, "expected { at the start of a block")?;

        let mut statements = Vec::new();

        while !self.is_at_end() && self.curr_token_type() != Some(&TokenType::RightBrace) {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt)
            }
        }

        self.consume(TokenType::RightBrace, "expected } at the end of a block")?;
        Ok(statements)
    }

    fn print_statement(&mut self) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;

        self.consume(TokenType::Semicolon, "semicolon")?;
        Ok(Statement::Print(expr))
    }

    fn expression_statement(&mut self) -> Result<Statement, ParsingError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "semicolon")?;
        Ok(Statement::Expression(expr))
    }

    fn assignment(&mut self) -> Result<Expression, ParsingError> {
        let expr = self.or()?;

        if let Some(TokenType::Equal) = self.curr_token_type() {
            self.advance();
            let value = self.assignment()?;
            let line = expr
                .line
                .expect("expression should have line in source code");

            match expr.expr_type {
                ExpressionType::Variable(name) => {
                    let assign = self
                        .make_expr_with_line(ExpressionType::Assign(name, Box::new(value)), line);
                    Ok(assign)
                }
                ExpressionType::Get { object, name } => {
                    let set = self.make_expr_with_line(
                        ExpressionType::Set {
                            object,
                            name,
                            value: Box::new(value),
                        },
                        line,
                    );
                    Ok(set)
                }
                _ => Err(ParsingError::IllegalAssignmentTarget),
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.and()?;

        while let Some(Token {
            line,
            token_type: TokenType::Or,
        }) = self.curr_token()
        {
            let line = *line;
            self.advance();

            let right = self.expression()?;
            expr = self.make_expr_with_line(
                ExpressionType::LogicalOr(Box::new(expr), Box::new(right)),
                line,
            )
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.equality()?;

        while let Some(Token {
            line,
            token_type: TokenType::And,
        }) = self.curr_token()
        {
            let line = *line;
            self.advance();

            let right = self.expression()?;
            expr = self.make_expr_with_line(
                ExpressionType::LogicalAnd(Box::new(expr), Box::new(right)),
                line,
            )
        }

        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expression, ParsingError> {
        self.assignment()
    }

    fn equality(&mut self) -> Result<Expression, ParsingError> {
        let mut expression = self.comparison()?;

        while let Some(Token {
            line,
            token_type: operator @ (TokenType::BangEqual | TokenType::EqualEqual),
        }) = self.curr_token()
        {
            let line = *line;
            let operator = match operator {
                TokenType::BangEqual => BinaryOperator::BangEqual,
                _ => BinaryOperator::EqualEqual,
            };

            self.advance();

            let right = self.comparison()?;
            expression = self.make_expr_with_line(
                ExpressionType::Binary(Box::new(expression), operator, Box::new(right)),
                line,
            );
        }

        Ok(expression)
    }

    fn comparison(&mut self) -> Result<Expression, ParsingError> {
        let mut expression = self.term()?;

        while let Some(Token {
            line,
            token_type:
                operator @ (TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual),
        }) = self.curr_token()
        {
            let line = *line;
            let operator = match operator {
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                TokenType::Less => BinaryOperator::Less,
                _ => BinaryOperator::LessEqual,
            };

            self.advance();

            let right = self.term()?;
            expression = self.make_expr_with_line(
                ExpressionType::Binary(Box::new(expression), operator, Box::new(right)),
                line,
            );
        }

        Ok(expression)
    }

    fn term(&mut self) -> Result<Expression, ParsingError> {
        let mut expression = self.factor()?;

        while let Some(Token {
            token_type: operator @ (TokenType::Minus | TokenType::Plus),
            line,
        }) = self.curr_token()
        {
            let line = *line;
            let operator = match operator {
                TokenType::Minus => BinaryOperator::Minus,
                _ => BinaryOperator::Plus,
            };

            self.advance();

            let right = self.factor()?;
            expression = self.make_expr_with_line(
                ExpressionType::Binary(Box::new(expression), operator, Box::new(right)),
                line,
            );
        }

        Ok(expression)
    }

    fn factor(&mut self) -> Result<Expression, ParsingError> {
        let mut expression = self.unary()?;

        while let Some(Token {
            token_type: operator @ (TokenType::Slash | TokenType::Star),
            line,
        }) = self.curr_token()
        {
            let line = *line;
            let operator = match operator {
                TokenType::Slash => BinaryOperator::Slash,
                _ => BinaryOperator::Star,
            };

            self.advance();

            let right = self.factor()?;
            expression = self.make_expr_with_line(
                ExpressionType::Binary(Box::new(expression), operator, Box::new(right)),
                line,
            );
        }

        Ok(expression)
    }

    fn unary(&mut self) -> Result<Expression, ParsingError> {
        if let Some(Token {
            token_type: operator @ (TokenType::Bang | TokenType::Minus),
            line,
        }) = self.curr_token()
        {
            let line = *line;
            let operator = match operator {
                TokenType::Bang => UnaryOperator::Bang,
                _ => UnaryOperator::Minus,
            };
            self.advance();
            let right = self.unary()?;
            Ok(self.make_expr_with_line(ExpressionType::Unary(operator, Box::new(right)), line))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expression, ParsingError> {
        let mut expr = self.primary()?;

        loop {
            match self.curr_token() {
                Some(Token {
                    token_type: TokenType::LeftParen,
                    line,
                }) => {
                    let line = *line;
                    self.advance();
                    expr = self.finish_call(expr, line)?;
                }
                Some(Token {
                    token_type: TokenType::Dot,
                    line,
                }) => {
                    let line = *line;
                    self.advance();

                    let Some(TokenType::Identifier(name)) = self.curr_token_type() else {
                        return Err(self.unexpected_token("property name after '.'"));
                    };
                    let name = name.clone();
                    self.advance();
                    expr = self.make_expr_with_line(
                        ExpressionType::Get {
                            object: Box::new(expr),
                            name: name.clone(),
                        },
                        line,
                    )
                }
                _ => {
                    break;
                }
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression, line: usize) -> Result<Expression, ParsingError> {
        let mut args = vec![];

        if self.curr_token_type() != Some(&TokenType::RightParen) {
            loop {
                if args.len() >= 255 {
                    return Err(ParsingError::TooManyArguments);
                }
                args.push(self.expression()?);

                if self.curr_token_type() == Some(&TokenType::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, ")")?;

        Ok(self.make_expr_with_line(
            ExpressionType::Call {
                callee: Box::new(callee),
                args,
            },
            line,
        ))
    }

    fn primary(&mut self) -> Result<Expression, ParsingError> {
        let Some(Token { token_type, line }) = self.curr_token() else {
            return Err(self.unexpected_token("primary token"));
        };
        let line = *line;

        match token_type {
            TokenType::True
            | TokenType::False
            | TokenType::Nil
            | TokenType::Number(_)
            | TokenType::String(_) => {
                let literal = match token_type {
                    TokenType::False => Literal::Boolean(false),
                    TokenType::True => Literal::Boolean(true),
                    TokenType::Nil => Literal::Nil,
                    TokenType::Number(number) => Literal::Number(*number),
                    TokenType::String(str) => Literal::String(str.clone()),
                    _ => unreachable!(),
                };
                self.advance();
                Ok(self.make_expr_with_line(ExpressionType::Literal(literal), line))
            }
            TokenType::Identifier(name) => {
                let name = name.clone();
                self.advance();
                Ok(self.make_expr_with_line(ExpressionType::Variable(name), line))
            }
            TokenType::This => {
                self.advance();
                Ok(self.make_expr_with_line(ExpressionType::This, line))
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;

                self.consume(TokenType::RightParen, ")")?;
                Ok(self.make_expr_with_line(ExpressionType::Grouping(Box::new(expr)), line))
            }
            TokenType::Super => {
                self.advance();
                self.consume(TokenType::Dot, "'.' after 'super'")?;

                let Some(TokenType::Identifier(name)) = self.curr_token_type() else {
                    return Err(self.unexpected_token("superclass method name"));
                };
                let name = name.clone();
                self.advance();

                Ok(self.make_expr_with_line(ExpressionType::Super(name), line))
            }
            _ => Err(self.unexpected_token("primary token")),
        }
    }

    fn curr_token(&self) -> Option<&Token> {
        self.tokens.get(self.curr)
    }

    fn curr_token_type(&self) -> Option<&TokenType> {
        self.tokens.get(self.curr).map(|t| &t.token_type)
    }

    fn advance(&mut self) {
        self.curr += 1;
    }

    fn consume(&mut self, expected_token: TokenType, error_msg: &str) -> Result<(), ParsingError> {
        match self.curr_token() {
            Some(token) if token.token_type == expected_token => {
                self.advance();
                Ok(())
            }
            _ => Err(self.unexpected_token(error_msg)),
        }
    }

    fn make_expr(&mut self, expr_type: ExpressionType) -> Expression {
        let expr = Expression {
            id: self.next_id,
            line: None,
            expr_type,
        };

        self.next_id += 1;

        expr
    }

    fn make_expr_with_line(&mut self, expr_type: ExpressionType, line: usize) -> Expression {
        let expr = Expression {
            id: self.next_id,
            line: Some(line),
            expr_type,
        };

        self.next_id += 1;

        expr
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
            if self
                .tokens
                .get(self.curr - 1)
                .is_some_and(|t| t.token_type == TokenType::Semicolon)
            {
                return;
            }

            match self.curr_token() {
                Some(Token {
                    token_type:
                        TokenType::Class
                        | TokenType::Fun
                        | TokenType::Var
                        | TokenType::For
                        | TokenType::If
                        | TokenType::While
                        | TokenType::Print
                        | TokenType::Return,
                    ..
                }) => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn is_at_end(&mut self) -> bool {
        match self.curr_token_type() {
            Some(TokenType::EOF) | None => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
enum ParsingError {
    #[allow(dead_code)]
    UnexpectedToken(String),
    IllegalAssignmentTarget,
    TooManyArguments,
}
