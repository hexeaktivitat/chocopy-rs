use crate::token::Token;

// syntax tree here we go oh god
// manual implementation so I understand wtf is going on

// Type information

#[derive(Debug, Clone, PartialEq)]
pub enum Typed {
    Assigned(Type),
    Inferred(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    None,
    Empty,
    Bool,
    Num,
    Str,
    List(Box<Type>),
    Id(String),
}

// Expressions

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal, Option<Typed>),
    Unary(Unary),
    Binary(Binary),
    Identifier(Identifier),
    Assign(Assign),
    Logical(Logical),
    Grouping(Grouping),
    Call(Call),
    Variable(Variable),
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
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: Token,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub name: Token,
    pub value: Box<Expr>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping {
    pub expr: Box<Expr>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub args: Vec<Expr>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: Token,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub value: Box<Expr>,
    pub typed: Option<Typed>,
}

pub trait ExprVisitor<T, S> {
    fn visit_literal(&mut self, x: &mut Literal, state: S) -> T;
    fn visit_unary(&mut self, x: &mut Unary, state: S) -> T;
    fn visit_binary(&mut self, x: &mut Binary, state: S) -> T;
    fn visit_identifier(&mut self, x: &mut Identifier, state: S) -> T;
    fn visit_variable(&mut self, x: &mut Variable, state: S) -> T;
    fn visit_assign(&mut self, x: &mut Assign, state: S) -> T;
    fn visit_logical(&mut self, x: &mut Logical, state: S) -> T;
    fn visit_grouping(&mut self, x: &mut Grouping, state: S) -> T;
    fn visit_call(&mut self, x: &mut Call, state: S) -> T;
    fn visit_index(&mut self, x: &mut Index, state: S) -> T;
}

pub fn walk_expr<T, S>(mut visitor: impl ExprVisitor<T, S>, x: &mut Expr, state: S) -> T {
    match x {
        Expr::Literal(y, _) => visitor.visit_literal(y, state),
        Expr::Unary(y) => visitor.visit_unary(y, state),
        Expr::Binary(y) => visitor.visit_binary(y, state),
        Expr::Identifier(y) => visitor.visit_identifier(y, state),
        Expr::Variable(y) => visitor.visit_variable(y, state),
        Expr::Assign(y) => visitor.visit_assign(y, state),
        Expr::Logical(y) => visitor.visit_logical(y, state),
        Expr::Grouping(y) => visitor.visit_grouping(y, state),
        Expr::Call(y) => visitor.visit_call(y, state),
        Expr::Index(y) => visitor.visit_index(y, state),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Var(Var),
    Func(Func),
    Expression(Expression),
    Block(Block),
    If(If),
    Return(Return),
    Print(Print),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: Token,
    pub type_id: Token,
    pub initializer: Option<Box<Expr>>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: Token,
    pub type_id: Option<Token>,
    pub parameters: Vec<Stmt>,
    pub body: Box<Stmt>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expr: Expr,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub scope: Vec<Stmt>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Box<Expr>,
    pub then_branch: Box<Stmt>,
    pub elif_condition: Vec<Option<Expr>>,
    pub elif_branch: Vec<Option<Stmt>>,
    pub else_branch: Option<Box<Stmt>>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub keyword: Token,
    pub value: Option<Box<Expr>>,
    pub typed: Option<Typed>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Print {
    pub expr: Expr,
    pub typed: Option<Typed>,
}

pub trait StmtVisitor<T, S> {
    fn visit_var(&mut self, x: &mut Var, state: S) -> T;
    fn visit_func(&mut self, x: &mut Func, state: S) -> T;
    fn visit_expression(&mut self, x: &mut Expression, state: S) -> T;
    fn visit_block(&mut self, x: &mut Block, state: S) -> T;
    fn visit_if(&mut self, x: &mut If, state: S) -> T;
    fn visit_return(&mut self, x: &mut Return, state: S) -> T;
    fn visit_print(&mut self, x: &mut Print, state: S) -> T;
}

pub fn walk_stmt<T, S>(mut visitor: impl StmtVisitor<T, S>, x: &mut Stmt, state: S) -> T {
    match x {
        Stmt::Var(y) => visitor.visit_var(y, state),
        Stmt::Func(y) => visitor.visit_func(y, state),
        Stmt::Expression(y) => visitor.visit_expression(y, state),
        Stmt::Block(y) => visitor.visit_block(y, state),
        Stmt::If(y) => visitor.visit_if(y, state),
        Stmt::Return(y) => visitor.visit_return(y, state),
        Stmt::Print(y) => visitor.visit_print(y, state),
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(Literal::Number(n), _) => n.fmt(f),
            Expr::Literal(Literal::String(s), _) => s.fmt(f),
            Expr::Literal(Literal::False, _) => false.fmt(f),
            Expr::Literal(Literal::None, _) => f.write_str("None"),
            Expr::Literal(Literal::Empty, _) => f.write_str("Empty"),
            Expr::Literal(Literal::Eol, _) => f.write_str("End of line"),
            Expr::Literal(Literal::True, _) => true.fmt(f),
            Expr::Literal(Literal::List(l), _) => f.write_fmt(format_args!("{:?}", l)),
            Expr::Logical(l) => f.write_fmt(format_args!("{} {} {}", l.left, l.operator, l.right)),
            Expr::Variable(v) => v.name.fmt(f),
            Expr::Assign(a) => f.write_fmt(format_args!("{} = {}", a.name, a.value)),
            Expr::Unary(Unary {
                operator,
                right,
                typed,
            }) => f.write_fmt(format_args!("{}{}", operator, right)),
            Expr::Call(c) => c.callee.fmt(f),
            Expr::Binary(Binary {
                left,
                operator,
                right,
                typed,
            }) => f.write_fmt(format_args!("({} {} {})", left, operator, right)),
            Expr::Grouping(Grouping { expr, typed }) => f.write_fmt(format_args!("({:?})", expr)),
            Expr::Identifier(i) => f.write_fmt(format_args!("{}", i.name.token)),
            Expr::Index(i) => f.write_fmt(format_args!("{}", i.value)),
        }
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Block(bl) => f.write_fmt(format_args!("{:?}", bl.scope)),
            Stmt::Expression(ex) => ex.expr.fmt(f),
            Stmt::If(if_) => f.write_fmt(format_args!(
                "{} then {} else {}",
                if_.condition,
                if_.then_branch,
                if_.else_branch.as_ref().unwrap(),
            )),
            Stmt::Func(func) => f.write_fmt(format_args!(
                "function {} | {}\n",
                func.name.token,
                func.type_id.as_ref().unwrap(),
            )),
            Stmt::Print(pr) => pr.expr.fmt(f),
            Stmt::Return(re) => re.keyword.fmt(f),
            Stmt::Var(va) => {
                f.write_fmt(format_args!("variable {} | {}", va.name.token, va.type_id))
            } // Stmt::While(wh) => f.write_fmt(format_args!("while {}", wh.condition)),
        }
    }
}
