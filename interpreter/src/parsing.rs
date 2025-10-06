use std::fmt::{Debug, Display};

use crate::scanning::Token;

fn parse(tokens: &Vec<Token>) {}

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
            Token::False => Expression::Literal(Literal::False),
            Token::True => Expression::Literal(Literal::True),
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

#[derive(Debug)]
enum Expression {
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Literal(Literal),
    Grouping(Box<Expression>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnaryOperator {
    Minus,
    Bang,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug)]
enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Star => write!(f, "*"),
            BinaryOperator::BangEqual => write!(f, "!="),
            BinaryOperator::EqualEqual => write!(f, "=="),
            BinaryOperator::Greater => write!(f, ">"),
            BinaryOperator::GreaterEqual => write!(f, ">="),
            BinaryOperator::Less => write!(f, "<"),
            BinaryOperator::LessEqual => write!(f, "<="),
            BinaryOperator::Slash => write!(f, "/"),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Minus => write!(f, "-"),
            UnaryOperator::Bang => write!(f, "!"),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(number) => write!(f, "{}", number),
            Literal::String(string) => write!(f, "{}", string),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unary(operator, expression) => write!(f, "({} {})", operator, expression),
            Self::Binary(expression1, operator, expression2) => {
                write!(f, "({} {} {})", operator, expression1, expression2)
            }
            Self::Literal(literal) => write!(f, "{}", literal),
            Self::Grouping(expression) => write!(f, "(group {})", expression),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_expression() {
        let expression = Expression::Binary(
            Box::new(Expression::Unary(
                UnaryOperator::Minus,
                Box::new(Expression::Literal(Literal::Number(123.0))),
            )),
            BinaryOperator::Star,
            Box::new(Expression::Grouping(Box::new(Expression::Literal(
                Literal::Number(45.67),
            )))),
        );

        let output = format!("{}", expression);
        assert_eq!(output, "(* (- 123) (group 45.67))");
    }
}
