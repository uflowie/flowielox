use crate::expressions::Expression;

pub enum Statement {
    Expression(Expression),
    Print(Expression),
}
