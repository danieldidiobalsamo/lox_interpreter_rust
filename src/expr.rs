use std::fmt::Display;

use uuid::Uuid;

use crate::token::{LiteralType, Token};
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Grouping(Grouping),
    Variable(Variable),
    Assign(Assign),
    Logical(Logical),
    Call(Call),
}

impl Expr {
    pub fn get_uuid(&self) -> Uuid {
        match self {
            Expr::Literal(expr) => expr.uuid,
            Expr::Binary(expr) => expr.uuid,
            Expr::Unary(expr) => expr.uuid,
            Expr::Grouping(expr) => expr.uuid,
            Expr::Variable(expr) => expr.uuid,
            Expr::Assign(expr) => expr.uuid,
            Expr::Logical(expr) => expr.uuid,
            Expr::Call(expr) => expr.uuid,
        }
    }
}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_uuid().hash(state);
    }
}

impl Eq for Expr {}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(_) => write!(f, "Literal"),
            Expr::Binary(_) => write!(f, "Binary"),
            Expr::Unary(_) => write!(f, "Unary"),
            Expr::Grouping(_) => write!(f, "Grouping"),
            Expr::Variable(_) => write!(f, "Variable"),
            Expr::Assign(_) => write!(f, "Assign"),
            Expr::Logical(_) => write!(f, "Logical"),
            Expr::Call(_) => write!(f, "Call"),
        }
    }
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn AstVisitor<T>) -> T {
        match self {
            Expr::Literal(literal) => visitor.visit_literal(literal),
            Expr::Binary(binary) => visitor.visit_binary_expr(binary),
            Expr::Unary(unary) => visitor.visit_unary_expr(unary),
            Expr::Grouping(grouping) => visitor.visit_grouping_expr(grouping),
            Expr::Variable(var) => visitor.visit_variable_expr(var),
            Expr::Assign(assign) => visitor.visit_assign_expr(assign),
            Expr::Logical(logical) => visitor.visit_logical_expr(logical),
            Expr::Call(call) => visitor.visit_call_expr(call),
        }
    }
}

pub trait AstVisitor<T> {
    fn visit_literal(&mut self, expr: &Literal) -> T;
    fn visit_binary_expr(&mut self, expr: &Binary) -> T;
    fn visit_grouping_expr(&mut self, expr: &Grouping) -> T;
    fn visit_unary_expr(&mut self, expr: &Unary) -> T;
    fn visit_variable_expr(&mut self, expr: &Variable) -> T;
    fn visit_assign_expr(&mut self, expr: &Assign) -> T;
    fn visit_logical_expr(&mut self, expr: &Logical) -> T;
    fn visit_call_expr(&mut self, expr: &Call) -> T;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub uuid: Uuid,
    pub value: LiteralType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub uuid: Uuid,
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Grouping {
    pub uuid: Uuid,
    pub expression: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub uuid: Uuid,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub uuid: Uuid,
    pub name: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assign {
    pub uuid: Uuid,
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Logical {
    pub uuid: Uuid,
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub uuid: Uuid,
    pub callee: Box<Expr>,
    pub paren: Token,
    pub arguments: Vec<Box<Expr>>,
}
