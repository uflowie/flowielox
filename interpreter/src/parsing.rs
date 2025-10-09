use crate::{
    expressions::{BinaryOperator, Expression, Literal, UnaryOperator},
    scanning::Token,
};

pub fn parse(tokens: &[Token]) -> Expression {
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

    fn parse(&mut self) -> Expression {
        self.expression()
    }

    fn expression(&mut self) -> Expression {
        self.equality()
    }

    fn equality(&mut self) -> Expression {
        let mut expression = self.comparison();

        while let Some(operator @ (Token::BangEqual | Token::EqualEqual)) =
            self.tokens.get(self.curr)
        {
            let operator = match operator {
                Token::BangEqual => BinaryOperator::BangEqual,
                _ => BinaryOperator::EqualEqual,
            };

            self.curr += 1;

            let right = self.comparison();
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }

        expression
    }

    fn comparison(&mut self) -> Expression {
        let mut expression = self.term();

        while let Some(
            operator @ (Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual),
        ) = self.tokens.get(self.curr)
        {
            let operator = match operator {
                Token::Greater => BinaryOperator::Greater,
                Token::GreaterEqual => BinaryOperator::GreaterEqual,
                Token::Less => BinaryOperator::Less,
                _ => BinaryOperator::LessEqual,
            };

            self.curr += 1;

            let right = self.term();
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }

        expression
    }

    fn term(&mut self) -> Expression {
        let mut expression = self.factor();

        while let Some(operator @ (Token::Minus | Token::Plus)) = self.tokens.get(self.curr) {
            let operator = match operator {
                Token::Minus => BinaryOperator::Minus,
                _ => BinaryOperator::Plus,
            };

            self.curr += 1;

            let right = self.factor();
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }

        expression
    }

    fn factor(&mut self) -> Expression {
        let mut expression = self.unary();

        while let Some(operator @ (Token::Slash | Token::Star)) = self.tokens.get(self.curr) {
            let operator = match operator {
                Token::Slash => BinaryOperator::Slash,
                _ => BinaryOperator::Star,
            };

            self.curr += 1;

            let right = self.factor();
            expression = Expression::Binary(Box::new(expression), operator, Box::new(right));
        }

        expression
    }

    fn unary(&mut self) -> Expression {
        if let Some(operator @ (Token::Bang | Token::Minus)) = self.tokens.get(self.curr) {
            let operator = match operator {
                Token::Bang => UnaryOperator::Bang,
                _ => UnaryOperator::Minus,
            };
            self.curr += 1;
            let right = self.unary();
            Expression::Unary(operator, Box::new(right))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expression {
        let Some(token) = self.tokens.get(self.curr) else {
            return Expression::Literal(Literal::Nil);
        };
        self.curr += 1;
        match token {
            Token::False => Expression::Literal(Literal::Boolean(false)),
            Token::True => Expression::Literal(Literal::Boolean(true)),
            Token::Nil => Expression::Literal(Literal::Nil),
            Token::Number(number) => Expression::Literal(Literal::Number(*number)),
            Token::String(str) => Expression::Literal(Literal::String(str.clone())),
            Token::LeftParen => {
                let expression = self.expression();
                if self.tokens.get(self.curr) != Some(&Token::RightParen) {
                    println!("expected right parenthesis");
                }
                Expression::Grouping(Box::new(expression))
            }
            _ => panic!(),
        }
    }
}
