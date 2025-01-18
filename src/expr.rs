use crate::token::{LiteralType, Token};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Grouping(Grouping),
    Variable(Variable),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn AstVisitor<T>) -> T {
        match self {
            Expr::Literal(literal) => visitor.visit_literal(literal),
            Expr::Binary(binary) => visitor.visit_binary_expr(binary),
            Expr::Unary(unary) => visitor.visit_unary_expr(unary),
            Expr::Grouping(grouping) => visitor.visit_grouping_expr(grouping),
            Expr::Variable(var) => visitor.visit_variable_expr(var),
        }
    }
}

pub trait AstVisitor<T> {
    fn visit_literal(&mut self, expr: &Literal) -> T;
    fn visit_binary_expr(&mut self, expr: &Binary) -> T;
    fn visit_grouping_expr(&mut self, expr: &Grouping) -> T;
    fn visit_unary_expr(&mut self, expr: &Unary) -> T;
    fn visit_variable_expr(&mut self, expr: &Variable) -> T;
}

#[derive(Debug, PartialEq)]
pub struct Literal {
    pub value: LiteralType,
}

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: Token,
}
