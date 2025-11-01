use std::{
    collections::HashMap,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    expressions::{BinaryOperator, Expression, ExpressionType, Literal, UnaryOperator},
    statements::Statement,
};

pub fn interpret(
    statements: &[Statement],
    resolved: &HashMap<u32, usize>,
) -> Result<(), EvaluationError> {
    let mut interpreter = Interpreter {
        statements,
        resolved,
        environments: Environments::new(),
    };

    let global = &mut interpreter.environments;
    global.define(String::from("clock"), Value::NativeFunction(|_| clock()));

    interpreter.interpret()
}

fn clock() -> Value {
    Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as f64,
    )
}

struct Interpreter<'a> {
    environments: Environments,
    statements: &'a [Statement],
    resolved: &'a HashMap<u32, usize>,
}

impl<'a> Interpreter<'a> {
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
                    if let Err(err) = self.execute(stmt) {
                        self.environments.end();
                        return Err(err);
                    }
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
            Statement::Function { name, params, body } => {
                self.environments.define(
                    name.clone(),
                    Value::Function {
                        env_id: self.environments.curr,
                        params: params.clone(),
                        body: body.clone(),
                    },
                );
            }
            Statement::Return(expr) => {
                let ret = match expr {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };
                return Err(EvaluationError::PotentiallyIllegalReturnStatement(ret));
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<Value, EvaluationError> {
        match expr.expr_type.as_ref() {
            ExpressionType::LogicalOr(left, right) => {
                let left = self.evaluate(&left)?;
                if left.is_truthy() {
                    Ok(left)
                } else {
                    self.evaluate(&right)
                }
            }
            ExpressionType::LogicalAnd(left, right) => {
                let left = self.evaluate(&left)?;
                if !left.is_truthy() {
                    Ok(left)
                } else {
                    self.evaluate(&right)
                }
            }
            ExpressionType::Unary(operator, expression) => {
                let right = self.evaluate(&expression)?;
                match (operator, right) {
                    (UnaryOperator::Minus, Value::Number(num)) => Ok(Value::Number(-num)),
                    (UnaryOperator::Bang, val) => Ok(Value::Boolean(!val.is_truthy())),
                    _ => Err(EvaluationError::TypeMismatch),
                }
            }
            ExpressionType::Binary(left, operator, right) => {
                let left = self.evaluate(&left)?;
                let right = self.evaluate(&right)?;
                match (operator, left, right) {
                    (BinaryOperator::Minus, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Number(left - right))
                    }
                    (BinaryOperator::Plus, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Number(left + right))
                    }
                    (BinaryOperator::Plus, Value::String(left), Value::String(right)) => {
                        Ok(Value::String(left + &right))
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
            ExpressionType::Literal(literal) => Ok(match literal {
                Literal::String(s) => Value::String(s.clone()),
                Literal::Number(num) => Value::Number(*num),
                Literal::Boolean(b) => Value::Boolean(*b),
                Literal::Nil => Value::Nil,
            }),
            ExpressionType::Grouping(expression) => self.evaluate(&expression),
            ExpressionType::Variable(name) => self
                .environments
                .get(&name, self.get_distance(expr))
                .cloned()
                .ok_or(EvaluationError::UndefinedVariable),
            ExpressionType::Assign(name, value_expr) => {
                let value = self.evaluate(&value_expr)?;
                match self
                    .environments
                    .assign(&name, &value, self.get_distance(expr))
                {
                    Ok(()) => Ok(value),
                    Err(err) => Err(err),
                }
            }
            ExpressionType::Call { callee, args } => {
                let callee = self.evaluate(&callee)?;

                let args = args
                    .iter()
                    .map(|arg| self.evaluate(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                self.call(&callee, &args)
            }
        }
    }

    fn call(&mut self, callee: &Value, args: &[Value]) -> Result<Value, EvaluationError> {
        match callee {
            Value::NativeFunction(func) => Ok(func(args)),
            Value::Function {
                params,
                body,
                env_id,
            } => {
                if params.len() != args.len() {
                    return Err(EvaluationError::InvalidNumberOfArgumentsPassed);
                }

                let old_env_id = self.environments.curr;
                self.environments.curr = *env_id;
                self.environments.start_new();

                for (param, arg) in params.iter().zip(args) {
                    self.environments.define(param.clone(), arg.clone());
                }

                for stmt in body {
                    match self.execute(stmt) {
                        Err(EvaluationError::PotentiallyIllegalReturnStatement(val)) => {
                            self.environments.curr = old_env_id;
                            return Ok(val);
                        }
                        Err(err) => {
                            self.environments.curr = old_env_id;
                            return Err(err);
                        }
                        _ => continue,
                    }
                }

                self.environments.curr = old_env_id;
                Ok(Value::Nil)
            }
            _ => Err(EvaluationError::InvalidCalleeType),
        }
    }

    fn get_distance(&self, expr: &Expression) -> Option<usize> {
        self.resolved.get(&expr.id).copied()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    NativeFunction(fn(&[Value]) -> Value),
    Function {
        env_id: usize,
        params: Vec<String>,
        body: Vec<Statement>,
    },
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
    InvalidCalleeType,
    InvalidNumberOfArgumentsPassed,
    PotentiallyIllegalReturnStatement(Value), // ;)
}

struct Environment {
    parent_id: Option<usize>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new(parent_id: usize) -> Self {
        Self {
            parent_id: Some(parent_id),
            values: HashMap::new(),
        }
    }

    fn root() -> Self {
        Self {
            parent_id: None,
            values: HashMap::new(),
        }
    }
}

struct Environments {
    curr: usize,
    envs: Vec<Environment>,
}

impl Environments {
    fn new() -> Self {
        Self {
            curr: 0,
            envs: vec![Environment::root()],
        }
    }

    fn define(&mut self, key: String, value: Value) {
        self.envs[self.curr].values.insert(key, value);
    }

    fn assign(
        &mut self,
        key: &str,
        value: &Value,
        dist: Option<usize>,
    ) -> Result<(), EvaluationError> {
        let id = self
            .get_id_of_ancestor(dist)
            .ok_or(EvaluationError::UndefinedVariable)?;

        let slot = self.envs[id].values.get_mut(key).expect(
            "find should have returned the id of an environment that contains the given key",
        );

        *slot = value.clone();
        Ok(())
    }

    fn get(&self, key: &str, dist: Option<usize>) -> Option<&Value> {
        let id = self.get_id_of_ancestor(dist)?;
        self.envs[id].values.get(key)
    }

    fn start_new(&mut self) {
        self.envs.push(Environment::new(self.curr));
        self.curr = self.envs.len() - 1
    }

    fn end(&mut self) {
        self.curr = self.envs[self.curr]
            .parent_id
            .expect("did not expect root scope to be closed")
    }

    fn get_id_of_ancestor(&self, dist: Option<usize>) -> Option<usize> {
        match dist {
            Some(dist) => {
                let mut curr_id = Some(self.curr);

                for _ in 0..dist {
                    curr_id = self.envs
                        [curr_id.expect("resolver should have caught undefined variables")]
                    .parent_id
                }

                curr_id
            }
            None => Some(0),
        }
    }
}
