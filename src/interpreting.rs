use std::{
    borrow::Cow,
    collections::HashMap,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    expressions::{BinaryOperator, Expression, ExpressionType, Literal, UnaryOperator},
    statements::{ClassStatement, FunctionStatement, Statement},
};

pub fn interpret<'a>(
    statements: &'a [Statement],
    resolved: &'a HashMap<u32, usize>,
) -> Result<(), EvaluationError<'a>> {
    let mut interpreter = Interpreter::new(statements, resolved);

    interpreter.define_native_function("clock".to_owned(), |_| clock());

    interpreter.interpret()
}

fn clock() -> Value<'static> {
    Value::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as f64,
    )
}

struct Interpreter<'a> {
    environments: Environments<'a>,
    statements: &'a [Statement],
    resolved: &'a HashMap<u32, usize>,
    instances: Vec<Instance<'a>>,
    classes: Vec<Class<'a>>,
    native_functions: Vec<fn(&[Value<'a>]) -> Value<'a>>,
}

impl<'a> Interpreter<'a> {
    fn new(statements: &'a [Statement], resolved: &'a HashMap<u32, usize>) -> Self {
        Self {
            statements,
            resolved,
            environments: Environments::new(),
            instances: vec![],
            classes: vec![],
            native_functions: vec![],
        }
    }

    fn interpret(mut self) -> Result<(), EvaluationError<'a>> {
        for statement in self.statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute(&mut self, statement: &'a Statement) -> Result<(), EvaluationError<'a>> {
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
            Statement::Function(stmt) => {
                self.environments.define(
                    stmt.name.clone(),
                    Value::Function(Function {
                        env_id: self.environments.curr,
                        stmt,
                        is_initializer: false,
                    }),
                );
            }
            Statement::Return(expr) => {
                let ret = match expr {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };
                return Err(EvaluationError::PotentiallyIllegalReturnStatement(ret));
            }
            Statement::Class(
                stmt @ ClassStatement {
                    name,
                    methods,
                    superclass,
                },
            ) => {
                let mut superclass_id = None;
                let enclosing_id = self.environments.curr;

                self.environments.define(name.clone(), Value::Nil);

                if let Some(superclass) = superclass {
                    let value = self.evaluate(superclass)?;
                    match value {
                        Value::ClassIdentifier(id) => {
                            superclass_id = Some(id);
                            self.environments.start_new();
                            self.environments.define("super".to_owned(), value);
                        }
                        _ => return Err(EvaluationError::SuperClassIsNotAClass),
                    }
                }

                let mut scoped_methods = HashMap::new();

                for method in methods {
                    scoped_methods.insert(
                        method.name.as_ref(),
                        Function {
                            stmt: &method,
                            env_id: self.environments.curr,
                            is_initializer: method.name == "init",
                        },
                    );
                }

                let class = Class {
                    stmt: stmt,
                    methods: scoped_methods,
                    superclass_id,
                };
                let class_id = Value::ClassIdentifier(self.classes.len());
                self.classes.push(class);

                self.environments.curr = enclosing_id;
                self.environments.assign(&name, class_id.clone(), Some(0))?;
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expr: &'a Expression) -> Result<Value<'a>, EvaluationError<'a>> {
        match &expr.expr_type {
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
                        let mut concatenated = left.into_owned();
                        concatenated.push_str(right.as_ref());
                        Ok(Value::String(Cow::Owned(concatenated)))
                    }
                    (BinaryOperator::Star, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Number(left * right))
                    }
                    (BinaryOperator::Slash, Value::Number(left), Value::Number(right)) => {
                        Ok(Value::Number(left / right))
                    }
                    (BinaryOperator::BangEqual, left, right) => Ok(Value::Boolean(left != right)),
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
                Literal::String(s) => Value::String(Cow::Borrowed(&s)),
                Literal::Number(num) => Value::Number(*num),
                Literal::Boolean(b) => Value::Boolean(*b),
                Literal::Nil => Value::Nil,
            }),
            ExpressionType::Grouping(expression) => self.evaluate(&expression),
            ExpressionType::Variable(name) => self.lookup(expr, &name),
            ExpressionType::Assign(name, value_expr) => {
                let value = self.evaluate(&value_expr)?;
                match self
                    .environments
                    .assign(&name, value.clone(), self.get_distance(expr))
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
            ExpressionType::Get { object, name } => {
                let instance_id = self.try_get_instance_id(&object)?;
                let instance = &self.instances[instance_id];

                if let Some(val) = instance.fields.get(name.as_str()) {
                    return Ok(val.clone());
                }

                if let Some(class_id) =
                    self.get_id_of_class_containing_method(instance.class_id, name)
                {
                    let class = &self.classes[class_id];
                    let method = Self::bind(
                        &mut self.environments,
                        instance_id,
                        &class.methods.get(name.as_str()).expect("get_id_of_class_containing_method should have returned the id of a class containing the method"),
                    );
                    Ok(Value::Function(method))
                } else {
                    Err(EvaluationError::UndefinedProperty)
                }
            }
            ExpressionType::Set {
                object,
                name,
                value,
            } => {
                let id = self.try_get_instance_id(&object)?;
                let value = self.evaluate(&value)?;
                let instance = &mut self.instances[id];

                instance.fields.insert(&name, value.clone());
                Ok(value)
            }
            ExpressionType::This => self.lookup(expr, "this"),
            ExpressionType::Super(name) => {
                let distance = self
                    .get_distance(expr)
                    .expect("super cant exist in global scope");

                let Value::ClassIdentifier(superclass_id) = self
                    .environments
                    .get("super", Some(distance))
                    .expect("super should have been defined")
                else {
                    panic!("super should have been a class identifier");
                };

                let Value::InstanceIdentifier(instance_id) = self
                    .environments
                    .get("this", Some(distance - 1))
                    .expect("this should have been defined")
                else {
                    panic!("this should have been an instance identifier");
                };
                let instance_id = *instance_id;

                match self.get_id_of_class_containing_method(*superclass_id, name.as_str()) {
                    Some(id) => {
                        let class = &self.classes[id];
                        let method = Self::bind(
                        &mut self.environments,
                        instance_id,
                        &class.methods.get(name.as_str()).expect("get_id_of_class_containing_method should have returned the id of a class containing the method"),
                    );
                        Ok(Value::Function(method))
                    }
                    None => Err(EvaluationError::UndefinedProperty),
                }
            }
        }
    }

    fn get_id_of_class_containing_method(&self, class_id: usize, name: &str) -> Option<usize> {
        let mut curr_id = Some(class_id);

        while let Some(id) = curr_id {
            let class = &self.classes[id];
            if let Some(_) = class.methods.get(name) {
                return Some(id);
            }
            curr_id = class.superclass_id
        }
        None
    }

    fn bind(envs: &mut Environments<'a>, instance_id: usize, func: &Function<'a>) -> Function<'a> {
        let original_env = envs.curr;

        envs.curr = func.env_id;
        envs.start_new();
        envs.define("this".to_owned(), Value::InstanceIdentifier(instance_id));

        let method = Function {
            env_id: envs.curr,
            stmt: func.stmt,
            is_initializer: func.is_initializer,
        };

        envs.curr = original_env;

        method
    }

    fn lookup(&self, expr: &Expression, name: &str) -> Result<Value<'a>, EvaluationError<'a>> {
        self.environments
            .get(&name, self.get_distance(expr))
            .cloned()
            .ok_or(EvaluationError::UndefinedVariable)
    }

    fn try_get_instance_id(&mut self, expr: &'a Expression) -> Result<usize, EvaluationError<'a>> {
        match self.evaluate(expr)? {
            Value::InstanceIdentifier(id) => Ok(id),
            _ => Err(EvaluationError::IllegalPropertyAccessTarget),
        }
    }

    fn call(
        &mut self,
        callee: &Value<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, EvaluationError<'a>> {
        match callee {
            Value::Function(Function {
                env_id,
                stmt,
                is_initializer,
            }) => {
                let FunctionStatement { params, body, .. } = stmt;

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
                    match self.execute(&stmt) {
                        Err(EvaluationError::PotentiallyIllegalReturnStatement(val)) => {
                            let result = if *is_initializer {
                                self.environments
                                    .get("this", Some(1)) // Some(1) to walk back out of the execution scope of the function into its closure
                                    .cloned()
                                    .expect("'this' should be defined in initializer")
                            } else {
                                val
                            };
                            self.environments.curr = old_env_id;
                            return Ok(result);
                        }
                        Err(err) => {
                            self.environments.curr = old_env_id;
                            return Err(err);
                        }
                        _ => continue,
                    }
                }

                let result = if *is_initializer {
                    self.environments
                        .get("this", Some(1))
                        .cloned()
                        .expect("'this' should be defined in initializer")
                } else {
                    Value::Nil
                };
                self.environments.curr = old_env_id;

                Ok(result)
            }
            Value::ClassIdentifier(id) => {
                let instance = Instance {
                    class_id: *id,
                    fields: HashMap::new(),
                };

                let instance_id = self.instances.len();
                self.instances.push(instance);

                let class = &self.classes[*id];

                if let Some(initializer) = class.methods.get("init") {
                    let method = Self::bind(&mut self.environments, instance_id, &initializer);
                    self.call(&Value::Function(method), args)?;
                }

                Ok(Value::InstanceIdentifier(instance_id))
            }
            Value::NativeFunctionIdentifier(id) => {
                let native_func = self.native_functions[*id];
                Ok(native_func(args))
            }
            _ => Err(EvaluationError::InvalidCalleeType),
        }
    }

    fn get_distance(&self, expr: &Expression) -> Option<usize> {
        self.resolved.get(&expr.id).copied()
    }

    fn define_native_function(&mut self, name: String, function: fn(&[Value<'a>]) -> Value<'a>) {
        self.environments.define(
            name,
            Value::NativeFunctionIdentifier(self.native_functions.len()),
        );
        self.native_functions.push(function);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Number(f64),
    String(Cow<'a, str>),
    Boolean(bool),
    Nil,
    Function(Function<'a>),
    // we use an identifier here because class instances need a reference to their class. a reference to the class statement is not enough
    // because the class statement doesn't capture the closure of the class.
    ClassIdentifier(usize),
    NativeFunctionIdentifier(usize), // we use an identifier here to avoid PartialEq warnings when using the fn pointer directly
    InstanceIdentifier(usize), // we use an identifier here to allow for mutability of instances without RefCell
}

#[derive(Debug, PartialEq, Clone)]
struct Class<'a> {
    stmt: &'a ClassStatement,
    methods: HashMap<&'a str, Function<'a>>,
    superclass_id: Option<usize>,
}

struct Instance<'a> {
    class_id: usize,
    fields: HashMap<&'a str, Value<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function<'a> {
    env_id: usize,
    stmt: &'a FunctionStatement,
    is_initializer: bool,
}

impl Value<'_> {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(false) | Value::Nil => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub enum EvaluationError<'a> {
    TypeMismatch,
    UndefinedVariable,
    InvalidCalleeType,
    InvalidNumberOfArgumentsPassed,
    PotentiallyIllegalReturnStatement(Value<'a>), // ;)
    IllegalPropertyAccessTarget,
    UndefinedProperty,
    SuperClassIsNotAClass,
}

struct Environment<'a> {
    parent_id: Option<usize>,
    values: HashMap<String, Value<'a>>,
}

impl Environment<'_> {
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

struct Environments<'a> {
    curr: usize,
    envs: Vec<Environment<'a>>,
}

impl<'a> Environments<'a> {
    fn new() -> Self {
        Self {
            curr: 0,
            envs: vec![Environment::root()],
        }
    }

    fn define(&mut self, key: String, value: Value<'a>) {
        self.envs[self.curr].values.insert(key, value);
    }

    fn assign(
        &mut self,
        key: &str,
        value: Value<'a>,
        dist: Option<usize>,
    ) -> Result<(), EvaluationError<'a>> {
        let id = self
            .get_id_of_ancestor(dist)
            .ok_or(EvaluationError::UndefinedVariable)?;

        let slot = self.envs[id].values.get_mut(key).expect(
            "find should have returned the id of an environment that contains the given key",
        );

        *slot = value;
        Ok(())
    }

    fn get(&self, key: &str, dist: Option<usize>) -> Option<&Value<'a>> {
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
