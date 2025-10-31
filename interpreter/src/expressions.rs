use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionType {
    Assign(String, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    LogicalOr(Box<Expression>, Box<Expression>),
    LogicalAnd(Box<Expression>, Box<Expression>),
    Literal(Literal),
    Grouping(Box<Expression>),
    Variable(String),
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub id: u32,
    pub line: Option<usize>, // option, because not every expression comes from source code, eg expressions created during desugaring
    pub expr_type: Box<ExpressionType>,
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

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.expr_type.fmt(f)
    }
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

impl Display for ExpressionType {
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
            Self::LogicalOr(left, right) => write!(f, "({} or {})", left, right),
            Self::LogicalAnd(left, right) => write!(f, "({} and {})", left, right),
            Self::Call { callee, args } => write!(f, "({}({:?}))", callee, args),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_expression() {
        let expression = ExpressionType::Binary(
            make_expr_without_metadata(ExpressionType::Unary(
                UnaryOperator::Minus,
                make_expr_without_metadata(ExpressionType::Literal(Literal::Number(123.0))),
            )),
            BinaryOperator::Star,
            make_expr_without_metadata(ExpressionType::Grouping(make_expr_without_metadata(
                ExpressionType::Literal(Literal::Number(45.67)),
            ))),
        );

        let output = format!("{}", expression);
        assert_eq!(output, "(* (- 123) (group 45.67))");
    }

    fn make_expr_without_metadata(expr_type: ExpressionType) -> Box<Expression> {
        Box::new(Expression {
            expr_type: Box::new(expr_type),
            id: 0,
            line: None,
        })
    }
}
