use std::fmt::{Debug, Display};

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOperator {
    Plus,
    Minus,
    Star,
}

#[derive(Debug)]
enum Literal {
    Number(f64),
    String(String),
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => write!(f, "-"),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(number) => write!(f, "{}", number),
            Self::String(string) => write!(f, "{:?}", string),
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
