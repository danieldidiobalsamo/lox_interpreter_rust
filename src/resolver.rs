use std::collections::HashMap;

use thiserror::Error;

use crate::{
    expr::{AstVisitor, Expr},
    interpreter::Interpreter,
    stmt::{Stmt, StmtVisitor},
    token::{LiteralType, Token},
};

#[derive(Debug, Clone, Error)]
pub enum ResolverError {
    #[error("Already a variable with this name in this scope")]
    SameVarNameSameScope,
    #[error("Can't read local variable in its own initializer.")]
    ReadVarOwnInit,
    #[error("{keyword} : can't use 'super' outside of a class.")]
    SuperOutsideClass { keyword: String },
    #[error("{keyword} : can't use 'super' in a class with no superclass.")]
    SuperOutsideWithoutSuperClass { keyword: String },
    #[error("Can't return from top-level code.")]
    ReturnFromTopLevel,
    #[error("Can't return a value from an initializer.")]
    ReturnFromInit,
    #[error("A class can't inherit from itself.")]
    InheritSelf,
    #[error("Can't use 'this' outside of a class.")]
    ThisOutsideClass,
}

#[derive(Debug, Clone, Default, PartialEq)]
enum FunctionType {
    #[default]
    None,
    Function,
    Method,
    Initializer,
}

#[derive(Debug, Clone, Default, PartialEq)]
enum ClassType {
    #[default]
    None,
    Class,
    Subclass,
}

pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve(&mut self, statements: &[Stmt]) -> Result<(), ResolverError> {
        for statement in statements {
            self.resolve_stmt(statement)?;
        }

        Ok(())
    }

    fn resolve_box_stmt(&mut self, statements: &Vec<Stmt>) -> Result<(), ResolverError> {
        for statement in statements {
            self.resolve_stmt(statement)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, statement: &Stmt) -> Result<(), ResolverError> {
        statement.accept(self)?;
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolverError> {
        expr.accept(self)?;

        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        let n = self.scopes.len();

        for i in (0..n).rev() {
            if let Some(map) = self.scopes.get(i) {
                if map.contains_key(&name.get_lexeme()) {
                    self.interpreter.resolve(expr, n - 1 - i);
                }
            }
        }
    }

    fn resolve_function(
        &mut self,
        function: &crate::stmt::Function,
        function_type: FunctionType,
    ) -> Result<(), ResolverError> {
        let enclosing_function = self.current_function.clone();
        self.current_function = function_type;

        self.begin_scope();

        for param in &function.params {
            self.declare(param)?;
            self.define(param);
        }

        self.resolve_box_stmt(&function.body)?;

        self.end_scope();

        self.current_function = enclosing_function;

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.get_lexeme()) {
                return Err(ResolverError::SameVarNameSameScope);
            }

            scope.insert(name.get_lexeme(), false);
        }

        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.get_lexeme(), true);
        }
    }
}

impl AstVisitor<Result<(), ResolverError>> for Resolver<'_> {
    fn visit_literal(&mut self, _expr: &crate::expr::Literal) -> Result<(), ResolverError> {
        Ok(())
    }

    fn visit_binary_expr(&mut self, expr: &crate::expr::Binary) -> Result<(), ResolverError> {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)?;

        Ok(())
    }

    fn visit_grouping_expr(&mut self, expr: &crate::expr::Grouping) -> Result<(), ResolverError> {
        self.resolve_expr(&expr.expression)?;
        Ok(())
    }

    fn visit_unary_expr(&mut self, expr: &crate::expr::Unary) -> Result<(), ResolverError> {
        self.resolve_expr(&expr.right)?;
        Ok(())
    }

    fn visit_variable_expr(&mut self, expr: &crate::expr::Variable) -> Result<(), ResolverError> {
        if let Some(map) = self.scopes.last() {
            if let Some(val) = map.get(&expr.name.get_lexeme()) {
                if !(*val) {
                    return Err(ResolverError::ReadVarOwnInit);
                }
            }
        }
        self.resolve_local(&Expr::Variable(expr.clone()), &expr.name);

        Ok(())
    }

    fn visit_assign_expr(&mut self, expr: &crate::expr::Assign) -> Result<(), ResolverError> {
        self.resolve_expr(&expr.value)?;
        self.resolve_local(&Expr::Assign(expr.clone()), &expr.name);

        Ok(())
    }

    fn visit_logical_expr(&mut self, expr: &crate::expr::Logical) -> Result<(), ResolverError> {
        self.resolve_expr(&expr.left)?;
        self.resolve_expr(&expr.right)?;

        Ok(())
    }

    fn visit_call_expr(&mut self, expr: &crate::expr::Call) -> Result<(), ResolverError> {
        self.resolve_expr(&expr.callee)?;

        for arg in &expr.arguments {
            self.resolve_expr(arg)?;
        }

        Ok(())
    }

    fn visit_get_expr(&mut self, expr: &crate::expr::Get) -> Result<(), ResolverError> {
        self.resolve_expr(&expr.object)?;

        Ok(())
    }

    fn visit_set_expr(&mut self, expr: &crate::expr::Set) -> Result<(), ResolverError> {
        self.resolve_expr(&expr.value)?;
        self.resolve_expr(&expr.object)?;

        Ok(())
    }

    fn visit_this_expr(&mut self, expr: &crate::expr::This) -> Result<(), ResolverError> {
        if self.current_class == ClassType::None {
            return Err(ResolverError::ThisOutsideClass);
        }

        self.resolve_local(&Expr::This(expr.clone()), &expr.keyword);
        Ok(())
    }

    fn visit_super_expr(&mut self, expr: &crate::expr::SuperExpr) -> Result<(), ResolverError> {
        match self.current_class {
            ClassType::Subclass => {
                self.resolve_local(&Expr::SuperExpr(expr.clone()), &expr.keyword);
                Ok(())
            }
            ClassType::None => Err(ResolverError::SuperOutsideClass {
                keyword: expr.keyword.get_lexeme(),
            }),

            _ => Err(ResolverError::SuperOutsideWithoutSuperClass {
                keyword: expr.keyword.get_lexeme(),
            }),
        }
    }
}

impl StmtVisitor<Result<(), ResolverError>> for Resolver<'_> {
    fn visit_expression(&mut self, stmt: &crate::stmt::Expression) -> Result<(), ResolverError> {
        self.resolve_expr(&stmt.expression)?;
        Ok(())
    }

    fn visit_print(&mut self, stmt: &crate::stmt::Print) -> Result<(), ResolverError> {
        self.resolve_expr(&stmt.expression)?;
        Ok(())
    }

    fn visit_var(&mut self, stmt: &crate::stmt::Var) -> Result<(), ResolverError> {
        self.declare(&stmt.name)?;

        if let Some(ref init) = *stmt.initializer {
            self.resolve_expr(init)?;
        }

        self.define(&stmt.name);

        Ok(())
    }

    fn visit_block(&mut self, stmt: &crate::stmt::Block) -> Result<(), ResolverError> {
        self.begin_scope();
        self.resolve_box_stmt(&stmt.statements)?;
        self.end_scope();

        Ok(())
    }

    fn visit_if(&mut self, stmt: &crate::stmt::If) -> Result<(), ResolverError> {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.then_branch)?;

        if let Some(ref else_branch) = *stmt.else_branch {
            self.resolve_stmt(else_branch)?;
        }

        Ok(())
    }

    fn visit_while(&mut self, stmt: &crate::stmt::While) -> Result<(), ResolverError> {
        self.resolve_expr(&stmt.condition)?;
        self.resolve_stmt(&stmt.body)?;

        Ok(())
    }

    fn visit_function(&mut self, stmt: &crate::stmt::Function) -> Result<(), ResolverError> {
        self.declare(&stmt.name)?;
        self.define(&stmt.name);
        self.resolve_function(stmt, FunctionType::Function)?;

        Ok(())
    }

    fn visit_return(&mut self, stmt: &crate::stmt::Return) -> Result<(), ResolverError> {
        if self.current_function == FunctionType::None {
            return Err(ResolverError::ReturnFromTopLevel);
        }

        if let Some(ref value) = *stmt.value {
            if self.current_function == FunctionType::Initializer {
                if let Expr::Literal(value_expr) = value {
                    if value_expr.value != LiteralType::Nil {
                        return Err(ResolverError::ReturnFromInit);
                    }
                }
            }

            self.resolve_expr(value)?;
        }

        Ok(())
    }

    fn visit_class(&mut self, stmt: &crate::stmt::Class) -> Result<(), ResolverError> {
        let enclosing_class = self.current_class.clone();
        self.current_class = ClassType::Class;

        self.declare(&stmt.name)?;
        self.define(&stmt.name);

        if let Some(Expr::Variable(ref super_class)) = *stmt.super_class {
            if stmt.name.get_lexeme() == *super_class.name.get_lexeme() {
                return Err(ResolverError::InheritSelf);
            }

            self.current_class = ClassType::Subclass;

            self.resolve_expr(&Expr::Variable(super_class.clone()))?;

            self.begin_scope();
            self.scopes
                .last_mut()
                .unwrap()
                .insert("super".to_owned(), true);
        }

        self.begin_scope();
        self.scopes
            .last_mut()
            .unwrap()
            .insert("this".to_owned(), true);

        for method in &stmt.methods {
            if let Stmt::Function(m) = &method {
                let declaration = if m.name.get_lexeme() == "init" {
                    FunctionType::Initializer
                } else {
                    FunctionType::Method
                };

                self.resolve_function(m, declaration)?;
            }
        }

        self.end_scope();

        if stmt.super_class.is_some() {
            self.end_scope();
        }

        self.current_class = enclosing_class;

        Ok(())
    }
}
