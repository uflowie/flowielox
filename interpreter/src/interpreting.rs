use std::collections::HashMap;

use crate::{
    expressions::{BinaryOperator, Expression, Literal, UnaryOperator},
    statements::Statement,
};

pub fn interpret(statements: &[Statement]) {
    let interpreter = Interpreter::new(statements);
    interpreter.interpret();
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

    fn interpret(mut self) {
        for statement in self.statements {
            self.execute(statement);
        }
    }

    fn execute(&mut self, statement: &Statement) {
        match statement {
            Statement::Expression(expr) => {
                self.evaluate(expr);
            }
            Statement::Print(expr) => {
                let value = self.evaluate(expr).unwrap();
                println!("{:?}", value);
            }
            Statement::Variable { name, initializer } => {
                let value = if let Some(expr) = initializer {
                    self.evaluate(expr).unwrap()
                } else {
                    Value::Nil
                };
                self.environments.define(name.clone(), value);
            }
            Statement::Block(stmts) => {
                self.environments.start_new();

                for stmt in stmts {
                    self.execute(stmt)
                }

                self.environments.end();
            }
        }
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<Value, EvaluationError> {
        match expression {
            Expression::Unary(operator, expression) => {
                let right = self.evaluate(expression)?;
                match (operator, right) {
                    (UnaryOperator::Minus, Value::Number(num)) => Ok(Value::Number(-num)),
                    (UnaryOperator::Bang, Value::Boolean(bool)) => Ok(Value::Boolean(!bool)),
                    (UnaryOperator::Bang, Value::Nil) => Ok(Value::Boolean(false)),
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
            Expression::Literal(literal) => {
                let value = match literal {
                    Literal::String(str) => Value::String(str.clone()),
                    Literal::Number(num) => Value::Number(*num),
                    Literal::Boolean(bool) => Value::Boolean(*bool),
                    Literal::Nil => Value::Nil,
                };
                Ok(value)
            }
            Expression::Grouping(expression) => self.evaluate(expression),
            Expression::Variable(name) => {
                if let Some(val) = self.environments.get(name) {
                    Ok(val.clone())
                } else {
                    Err(EvaluationError::UndefinedVariable)
                }
            }
            Expression::Assign(name, expr) => {
                let value = self.evaluate(expr)?;

                if let None = self.environments.assign(name, &value) {
                    Ok(value)
                } else {
                    Err(EvaluationError::UndefinedVariable)
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

#[derive(Debug)]
enum EvaluationError {
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
        for env in self.values.iter_mut().rev() {
            if let Some(val) = env.get_mut(key) {
                *val = value.clone();
                return None;
            }
        }
        return Some(EvaluationError::UndefinedVariable);
    }

    fn get(&self, key: &str) -> Option<&Value> {
        for env in self.values.iter().rev() {
            if let Some(val) = env.get(key) {
                return Some(val);
            }
        }
        return None;
    }

    fn start_new(&mut self) {
        self.values.push(HashMap::new())
    }

    fn end(&mut self) {
        self.values.pop();
    }
}
