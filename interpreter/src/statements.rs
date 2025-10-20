use crate::expressions::Expression;

pub enum Statement {
    Expression(Expression),
    Print(Expression),
    Variable {
        name: String,
        initializer: Option<Expression>,
    },
    Block(Vec<Statement>),
    If {
        then_branch: Box<Statement>,
        condition: Expression,
        else_branch: Option<Box<Statement>>,
    },
    While {
        condition: Expression,
        stmt: Box<Statement>,
    },
}
