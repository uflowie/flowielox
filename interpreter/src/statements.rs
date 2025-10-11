use crate::expressions::Expression;

pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Variable {
        name: String,
        initializer: Option<Expression>,
    },
}
