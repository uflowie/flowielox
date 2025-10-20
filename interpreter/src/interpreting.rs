use std::collections::HashMap;

use crate::{
    expressions::{BinaryOperator, Expression, Literal, UnaryOperator},
    statements::Statement,
};

pub fn interpret(statements: &[Statement]) -> Result<(), EvaluationError> {
    Interpreter::new(statements).interpret()
}

struct Interpreter<'a> {
    environments: EnvironmentStack,
    statements: &'a [Statement],
}

impl<'a> Interpreter<'a> {
    fn new(statements: &'a [Statement]) -> Self {
        Self {
            environments: EnvironmentStack::new(),
            statements,
        }
    }

    fn interpret(mut self) -> Result<(), EvaluationError> {
        for statement in self.statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, statement: &Statement) -> Result<(), EvaluationError> {
        match statement {
            Statement::Expression(expr) => {
                self.evaluate(expr)?;
            }
            Statement::Print(expr) => {
                let value = self.evaluate(expr)?;
                println!("{:?}", value);
            }
            Statement::Variable { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };
                self.environments.define(name.clone(), value);
            }
            Statement::Block(stmts) => {
                self.environments.start_new();

                for stmt in stmts {
                    self.execute(stmt)?;
                }

                self.environments.end();
            }
            Statement::If {
                then_branch,
                condition,
                else_branch,
            } => {
                if self.evaluate(condition)?.is_truthy() {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
            }
            Statement::While { condition, stmt } => {
                while self.evaluate(condition)?.is_truthy() {
                    self.execute(stmt)?
                }
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<Value, EvaluationError> {
        match expression {
            Expression::LogicalOr(left, right) => {
                let left = self.evaluate(left)?;
                if left.is_truthy() {
                    Ok(left)
                } else {
                    self.evaluate(right)
                }
            }
            Expression::LogicalAnd(left, right) => {
                let left = self.evaluate(left)?;
                if !left.is_truthy() {
                    Ok(left)
                } else {
                    self.evaluate(right)
                }
            }
            Expression::Unary(operator, expression) => {
                let right = self.evaluate(expression)?;
                match (operator, right) {
                    (UnaryOperator::Minus, Value::Number(num)) => Ok(Value::Number(-num)),
                    (UnaryOperator::Bang, val) => Ok(Value::Boolean(!val.is_truthy())),
                    _ => Err(EvaluationError::TypeMismatch),
                }
            }
            Expression::Binary(left, operator, right) => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                match (operator, left, right) {
                    (BinaryOperator::Minus, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Number(left - right))
                    }
                    (BinaryOperator::Plus, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Number(left + right))
                    }
                    (BinaryOperator::Minus, Value::String(left), Value::String(right)) => {
                        Ok(Value::String(left + &right))
                    }
                    (BinaryOperator::Star, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Number(left * right))
                    }
                    (BinaryOperator::Slash, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Number(left / right))
                    }
                    (BinaryOperator::BangEqual, Value::Boolean(left), Value::Boolean(right)) => {
                        Ok(Value::Boolean(left != right))
                    }
                    (BinaryOperator::EqualEqual, left, right) => Ok(Value::Boolean(left == right)),
                    (BinaryOperator::Greater, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Boolean(left > right))
                    }
                    (BinaryOperator::GreaterEqual, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Boolean(left >= right))
                    }
                    (BinaryOperator::Less, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Boolean(left < right))
                    }
                    (BinaryOperator::LessEqual, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Boolean(left <= right))
                    }
                    _ => Err(EvaluationError::TypeMismatch),
                }
            }
            Expression::Literal(literal) => Ok(match literal {
                Literal::String(s) => Value::String(s.clone()),
                Literal::Number(num) => Value::Number(*num),
                Literal::Boolean(b) => Value::Boolean(*b),
                Literal::Nil => Value::Nil,
            }),
            Expression::Grouping(expression) => self.evaluate(expression),
            Expression::Variable(name) => self
                .environments
                .get(name)
                .cloned()
                .ok_or(EvaluationError::UndefinedVariable),
            Expression::Assign(name, expr) => {
                let value = self.evaluate(expr)?;
                match self.environments.assign(name, &value) {
                    None => Ok(value),
                    Some(err) => Err(err),
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(false) | Value::Nil => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub enum EvaluationError {
    TypeMismatch,
    UndefinedVariable,
}

struct EnvironmentStack {
    values: Vec<HashMap<String, Value>>,
}

impl EnvironmentStack {
    fn new() -> Self {
        Self {
            values: vec![HashMap::new()],
        }
    }

    fn define(&mut self, key: String, value: Value) {
        self.values.last_mut().unwrap().insert(key, value);
    }

    fn assign(&mut self, key: &str, value: &Value) -> Option<EvaluationError> {
        if let Some(slot) = self
            .values
            .iter_mut()
            .rev()
            .find_map(|env| env.get_mut(key))
        {
            *slot = value.clone();
            None
        } else {
            Some(EvaluationError::UndefinedVariable)
        }
    }

    fn get(&self, key: &str) -> Option<&Value> {
        self.values.iter().rev().find_map(|env| env.get(key))
    }

    fn start_new(&mut self) {
        self.values.push(HashMap::new())
    }

    fn end(&mut self) {
        self.values.pop();
    }
}
