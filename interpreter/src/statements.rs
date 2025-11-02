use crate::expressions::Expression;

#[derive(Debug, PartialEq, Clone)]
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
    Function(FunctionStatement),
    Class(ClassStatement),
    Return(Option<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionStatement {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Statement>,
    pub function_type: FunctionType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassStatement {
    pub name: String,
    pub methods: Vec<FunctionStatement>,
    pub superclass: Option<Expression>
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionType {
    Function,
    Method,
}
