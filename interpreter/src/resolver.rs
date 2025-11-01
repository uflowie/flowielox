use std::collections::HashMap;

use crate::{
    expressions::{Expression, ExpressionType},
    statements::Statement,
};

pub fn resolve(stmts: &[Statement]) -> Result<HashMap<u32, usize>, ResolverError> {
    let resolver = Resolver {
        stmts,
        scopes: vec![],
        resolved: HashMap::new(),
        curr_function: FunctionType::None,
    };
    resolver.resolve()
}

struct Resolver<'a> {
    stmts: &'a [Statement],
    scopes: Vec<HashMap<String, bool>>,
    resolved: HashMap<u32, usize>,
    curr_function: FunctionType,
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
            Statement::Function { name, params, body } => {
                self.declare(name)?;
                self.define(name);
                self.resolve_function(params, body)?;
            }
            Statement::Return(expression) => {
                if self.curr_function == FunctionType::None {
                    return Err(ResolverError::TopLevelReturn);
                }

                if let Some(expression) = expression {
                    self.resolve_expr(expression)?;
                }
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Result<(), ResolverError> {
        match expr.expr_type.as_ref() {
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
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        params: &Vec<String>,
        body: &Vec<Statement>,
    ) -> Result<(), ResolverError> {
        let enclosing = self.curr_function.clone();
        self.curr_function = FunctionType::Function;
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
}

#[derive(Clone, PartialEq)]
enum FunctionType {
    None,
    Function,
}
