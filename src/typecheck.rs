use crate::syntax::*;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeChecker {
    pub ast: Vec<Stmt>,
    pub typed_ast: Vec<(Stmt, Type)>,
}
