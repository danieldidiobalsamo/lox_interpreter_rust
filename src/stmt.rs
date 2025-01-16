use crate::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expression(Expression),
    Print(Print),
}

pub trait StmtVisitor<T> {
    fn visit_expression(&mut self, expr: &Expression) -> T;
    fn visit_print(&mut self, expr: &Print) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn StmtVisitor<T>) -> T {
        match self {
            Stmt::Expression(expression) => visitor.visit_expression(&expression),
            Stmt::Print(print) => visitor.visit_print(&print),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub expression: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Print {
    pub expression: Box<Expr>,
}
