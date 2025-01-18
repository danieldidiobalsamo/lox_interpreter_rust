use crate::{expr::Expr, token::Token};

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Expression(Expression),
    Print(Print),
    Var(Var),
    Block(Block),
}

pub trait StmtVisitor<T> {
    fn visit_expression(&mut self, expr: &Expression) -> T;
    fn visit_print(&mut self, expr: &Print) -> T;
    fn visit_var(&mut self, expr: &Var) -> T;
    fn visit_block(&mut self, expr: &Block) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn StmtVisitor<T>) -> T {
        match self {
            Stmt::Expression(expression) => visitor.visit_expression(&expression),
            Stmt::Print(print) => visitor.visit_print(&print),
            Stmt::Var(var) => visitor.visit_var(var),
            Stmt::Block(block) => visitor.visit_block(block),
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

#[derive(Debug, PartialEq)]
pub struct Var {
    pub name: Token,
    pub initializer: Box<Option<Expr>>,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Box<Stmt>>,
}
