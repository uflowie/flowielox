use std::fmt::Display;

#[derive(Debug)]
pub enum Expression {
    Assign(String, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Literal(Literal),
    Grouping(Box<Expression>),
    Variable(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
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
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
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
            Literal::Boolean(true) => write!(f, "true"),
            Literal::Boolean(false) => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign(name, expression) => write!(f, "({} = {})", name, expression),
            Self::Unary(operator, expression) => write!(f, "({} {})", operator, expression),
            Self::Binary(expression1, operator, expression2) => {
                write!(f, "({} {} {})", operator, expression1, expression2)
            }
            Self::Literal(literal) => write!(f, "{}", literal),
            Self::Grouping(expression) => write!(f, "(group {})", expression),
            Self::Variable(_) => todo!(),
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
