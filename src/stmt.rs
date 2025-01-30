use crate::{
    expr::Expr,
    token::{LiteralType, Token},
};

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expression(Expression),
    Print(Print),
    Var(Var),
    Block(Block),
    If(If),
    While(While),
    Function(Function),
    Return(Return),
}

impl ToString for Stmt {
    fn to_string(&self) -> String {
        match self {
            Stmt::Expression(_) => "Expression".to_owned(),
            Stmt::Print(_) => "Print".to_owned(),
            Stmt::Var(_) => "Var".to_owned(),
            Stmt::Block(_) => "Block".to_owned(),
            Stmt::If(_) => "If".to_owned(),
            Stmt::While(_) => "While".to_owned(),
            Stmt::Function(_) => "Function".to_owned(),
            Stmt::Return(_) => "Return".to_owned(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Exit {
    Return(LiteralType),
    Error(String),
}

pub trait StmtVisitor<T> {
    fn visit_expression(&mut self, expr: &Expression) -> T;
    fn visit_print(&mut self, expr: &Print) -> T;
    fn visit_var(&mut self, expr: &Var) -> T;
    fn visit_block(&mut self, expr: &Block) -> T;
    fn visit_if(&mut self, expr: &If) -> T;
    fn visit_while(&mut self, expr: &While) -> T;
    fn visit_function(&mut self, expr: &Function) -> T;
    fn visit_return(&mut self, expr: &Return) -> T;
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn StmtVisitor<T>) -> T {
        match self {
            Stmt::Expression(expression) => visitor.visit_expression(&expression),
            Stmt::Print(print) => visitor.visit_print(&print),
            Stmt::Var(var) => visitor.visit_var(var),
            Stmt::Block(block) => visitor.visit_block(block),
            Stmt::If(condition) => visitor.visit_if(condition),
            Stmt::While(while_loop) => visitor.visit_while(while_loop),
            Stmt::Function(function) => visitor.visit_function(function),
            Stmt::Return(return_stmt) => visitor.visit_return(return_stmt),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub expression: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Print {
    pub expression: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub name: Token,
    pub initializer: Box<Option<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Box<Stmt>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub condition: Box<Expr>,
    pub then_branch: Box<Stmt>,
    pub else_branch: Box<Option<Stmt>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub condition: Box<Expr>,
    pub body: Box<Stmt>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Box<Stmt>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Return {
    pub keyword: Token,
    pub value: Box<Option<Expr>>,
}
