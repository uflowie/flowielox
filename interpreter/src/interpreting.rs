use crate::{
    expressions::{BinaryOperator, Expression, Literal, UnaryOperator},
    statements::Statement,
};

pub fn interpret(statements: &[Statement]) {
    for statement in statements {
        execute(statement);
    }
}

fn execute(statement: &Statement) {
    match statement {
        Statement::Expression(expr) => {
            evaluate(expr);
        }
        Statement::Print(expr) => {
            let value = evaluate(expr).unwrap();
            println!("{:?}", value);
        }
        Statement::Variable { name, initializer } => todo!(),
    }
}

fn evaluate(expression: &Expression) -> Result<Value, EvaluationError> {
    match expression {
        Expression::Unary(operator, expression) => {
            let right = evaluate(expression)?;
            match (operator, right) {
                (UnaryOperator::Minus, Value::Number(num)) => Ok(Value::Number(-num)),
                (UnaryOperator::Bang, Value::Boolean(bool)) => Ok(Value::Boolean(!bool)),
                (UnaryOperator::Bang, Value::Nil) => Ok(Value::Boolean(false)),
                _ => Err(EvaluationError::TypeMismatch),
            }
        }
        Expression::Binary(left, operator, right) => {
            let left = evaluate(left)?;
            let right = evaluate(right)?;
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
        Expression::Grouping(expression) => evaluate(expression),
        Expression::Variable(_) => todo!(),
    }
}

#[derive(Debug, PartialEq)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug)]
enum EvaluationError {
    TypeMismatch,
}
