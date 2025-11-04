use std::collections::HashMap;

use crate::{
    expressions::{Expression, ExpressionType},
    statements::{ClassStatement, FunctionStatement, Statement},
};

pub fn resolve(stmts: &[Statement]) -> Result<HashMap<u32, usize>, ResolverError> {
    let resolver = Resolver {
        stmts,
        scopes: vec![],
        resolved: HashMap::new(),
        curr_function: FunctionType::None,
        curr_class: ClassType::None,
    };
    resolver.resolve()
}

struct Resolver<'a> {
    stmts: &'a [Statement],
    scopes: Vec<HashMap<String, bool>>,
    resolved: HashMap<u32, usize>,
    curr_function: FunctionType,
    curr_class: ClassType,
}

impl<'a> Resolver<'a> {
    fn resolve(mut self) -> Result<HashMap<u32, usize>, ResolverError> {
        for stmt in self.stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(self.resolved)
    }

    fn resolve_stmt(&mut self, stmt: &Statement) -> Result<(), ResolverError> {
        match stmt {
            Statement::Expression(expression) => self.resolve_expr(expression)?,
            Statement::Print(expression) => self.resolve_expr(expression)?,
            Statement::Variable { name, initializer } => {
                self.declare(&name)?;

                if let Some(expr) = initializer {
                    self.resolve_expr(expr)?;
                }

                self.define(&name);
            }
            Statement::Block(statements) => {
                self.scopes.push(HashMap::new());

                for stmt in statements {
                    self.resolve_stmt(stmt)?;
                }

                self.scopes.pop();
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(&then_branch)?;
                if let Some(stmt) = else_branch {
                    self.resolve_stmt(stmt)?;
                }
            }
            Statement::While { condition, stmt } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(stmt)?;
            }
            Statement::Function(function) => {
                self.declare(&function.name)?;
                self.define(&function.name);
                self.resolve_function(function, FunctionType::Function)?;
            }
            Statement::Class(ClassStatement {
                name,
                methods,
                superclass,
            }) => {
                let enclosing = self.curr_class;

                self.curr_class = match superclass {
                    Some(_) => ClassType::Subclass,
                    None => ClassType::Class,
                };
                self.declare(name)?;
                self.define(name);

                if let Some(
                    expr @ Expression {
                        expr_type: ExpressionType::Variable(super_name),
                        ..
                    },
                ) = superclass
                {
                    if name == super_name {
                        return Err(ResolverError::SelfInheritance);
                    }
                    self.resolve_expr(expr)?;

                    self.scopes.push(HashMap::new());
                    self.define("super");
                }

                self.scopes.push(HashMap::new());

                self.define("this");

                for method in methods {
                    let declaration = match method.name.as_str() {
                        "init" => FunctionType::Initializer,
                        _ => FunctionType::Method,
                    };
                    self.resolve_function(method, declaration)?;
                }

                if superclass.is_some() {
                    self.scopes.pop();
                }

                self.scopes.pop();

                self.curr_class = enclosing;
            }
            Statement::Return(expression) => {
                return match self.curr_function {
                    FunctionType::None => Err(ResolverError::TopLevelReturn),
                    FunctionType::Initializer if expression.is_some() => {
                        Err(ResolverError::ReturnInInitializer)
                    }
                    _ => {
                        if let Some(expr) = expression {
                            self.resolve_expr(expr)?;
                        }
                        Ok(())
                    }
                };
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Result<(), ResolverError> {
        match &expr.expr_type {
            ExpressionType::Assign(name, expression) => {
                self.resolve_expr(expression)?;
                self.resolve_local(name, expr.id);
            }
            ExpressionType::Unary(_, expression) => self.resolve_expr(expression)?,
            ExpressionType::Binary(left, _, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            ExpressionType::LogicalOr(left, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            ExpressionType::LogicalAnd(left, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            ExpressionType::Literal(_) => {}
            ExpressionType::Grouping(expression) => self.resolve_expr(expression)?,
            ExpressionType::Variable(name) => {
                if self
                    .scopes
                    .last()
                    .map(|s| s.get(name))
                    .flatten()
                    .is_some_and(|x| !x)
                {
                    return Err(ResolverError::VariableReferencedInItsInitializer);
                }

                self.resolve_local(name, expr.id);
            }
            ExpressionType::Call { callee, args } => {
                self.resolve_expr(callee)?;

                for expr in args {
                    self.resolve_expr(expr)?;
                }
            }
            ExpressionType::Get { object, .. } => {
                self.resolve_expr(object)?;
            }
            ExpressionType::Set { object, value, .. } => {
                self.resolve_expr(&object)?;
                self.resolve_expr(&value)?;
            }
            ExpressionType::This => {
                if self.curr_class == ClassType::None {
                    return Err(ResolverError::ThisOutsideOfClass);
                }
                self.resolve_local("this", expr.id);
            }
            ExpressionType::Super(_) => {
                if self.curr_class != ClassType::Subclass {
                    return Err(ResolverError::SuperOutsideOfSubclass);
                }
                self.resolve_local("super", expr.id);
            }
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        function: &FunctionStatement,
        function_type: FunctionType,
    ) -> Result<(), ResolverError> {
        let FunctionStatement { params, body, .. } = function;
        let enclosing = self.curr_function;
        self.curr_function = function_type;
        self.scopes.push(HashMap::new());

        for param in params {
            self.declare(param)?;
            self.define(param);
        }

        for stmt in body {
            self.resolve_stmt(stmt)?;
        }

        self.scopes.pop();
        self.curr_function = enclosing;

        Ok(())
    }

    fn declare(&mut self, name: &str) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                return Err(ResolverError::VariableAlreadyDeclared);
            }
            scope.insert(name.to_string(), false);
        }
        Ok(())
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), true);
        }
    }

    fn resolve_local(&mut self, name: &str, id: u32) {
        if let Some((i, _)) = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find(|(_, scope)| scope.contains_key(name))
        {
            self.resolved.insert(id, self.scopes.len() - 1 - i);
        }
    }
}

#[derive(Debug)]
pub enum ResolverError {
    VariableReferencedInItsInitializer,
    VariableAlreadyDeclared,
    TopLevelReturn,
    ThisOutsideOfClass,
    ReturnInInitializer,
    SelfInheritance,
    SuperOutsideOfSubclass,
}

#[derive(Clone, Copy, PartialEq)]
enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
}

#[derive(Clone, Copy, PartialEq)]
enum ClassType {
    None,
    Class,
    Subclass,
}
