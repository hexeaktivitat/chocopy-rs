use crate::token::Token;

// syntax tree here we go oh god
// manual implementation so I understand wtf is going on

// Expressions

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
    Variable(Variable),
    Assign(Assign),
    Logical(Logical),
    Grouping(Grouping),
    Call(Call),
    Type(Type),
    Index(Index),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(i32),
    String(String),
    List(Vec<Expr>),
    True,
    False,
    None,
    Empty,
    Eol,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub name: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub value: Box<Expr>,
}

pub trait ExprVisitor<T, S> {
    fn visit_literal(&mut self, x: &Literal, state: S) -> T;
    fn visit_unary(&mut self, x: &Unary, state: S) -> T;
    fn visit_binary(&mut self, x: &Binary, state: S) -> T;
    fn visit_variable(&mut self, x: &Variable, state: S) -> T;
    fn visit_assign(&mut self, x: &Assign, state: S) -> T;
    fn visit_logical(&mut self, x: &Logical, state: S) -> T;
    fn visit_grouping(&mut self, x: &Grouping, state: S) -> T;
    fn visit_call(&mut self, x: &Call, state: S) -> T;
    fn visit_type(&mut self, x: &Type, state: S) -> T;
    fn visit_index(&mut self, x: &Index, state: S) -> T;
}

pub fn walk_expr<T, S>(mut visitor: impl ExprVisitor<T, S>, x: &Expr, state: S) -> T {
    match x {
        Expr::Literal(y) => visitor.visit_literal(y, state),
        Expr::Unary(y) => visitor.visit_unary(y, state),
        Expr::Binary(y) => visitor.visit_binary(y, state),
        Expr::Variable(y) => visitor.visit_variable(y, state),
        Expr::Assign(y) => visitor.visit_assign(y, state),
        Expr::Logical(y) => visitor.visit_logical(y, state),
        Expr::Grouping(y) => visitor.visit_grouping(y, state),
        Expr::Call(y) => visitor.visit_call(y, state),
        Expr::Type(y) => visitor.visit_type(y, state),
        Expr::Index(y) => visitor.visit_index(y, state),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expression(Expression),
    Block(Block),
    If(If),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub scope: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Box<Expr>,
    pub then_branch: Box<Stmt>,
    pub elif_condition: Vec<Option<Expr>>,
    pub elif_branch: Vec<Option<Stmt>>,
    pub else_branch: Option<Box<Stmt>>,
}
