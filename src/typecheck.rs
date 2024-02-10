use crate::syntax::{Expr, ExprVisitor, Stmt, StmtVisitor};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    None,
    Empty,
    Bool,
    Num,
    String,
    List(Box<Type>),
    Id(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeChecker {
    pub ast: Vec<Stmt>,
    pub typed_ast: Vec<(Stmt, Type)>,
}
