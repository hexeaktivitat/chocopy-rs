use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Num(i32),
    Str(String),
    Boolean(bool),
    List(Vec<Literal>),
    None,
    Empty,
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Num(n) => f.write_fmt(format_args!("NUM: {}", n)),
            Literal::Str(s) => f.write_fmt(format_args!("STR: {}", s)),
            Literal::Boolean(b) => f.write_fmt(format_args!("BOOL: {}", b)),
            Literal::List(l) => f.write_fmt(format_args!("LIST: {:?}", l)),
            Literal::None => f.write_str("<NONE>"),
            Literal::Empty => f.write_str("<EMPTY>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Equals,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Not,
    NotEquals,
    EqualEquals,
    GreaterThan,
    LesserThan,
    GreaterEqual,
    LesserEqual,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Equals => f.write_str("EQUALS"),
            Op::Add => f.write_str("ADD"),
            Op::Subtract => f.write_str("SUBTRACT"),
            Op::Multiply => f.write_str("MULTIPLY"),
            Op::Divide => f.write_str("DIVIDE"),
            Op::Remainder => f.write_str("REMAINDER"),
            Op::Not => f.write_str("NOT"),
            Op::NotEquals => f.write_str("NOT EQUAL"),
            Op::EqualEquals => f.write_str("EQUAL EQUALS"),
            Op::GreaterThan => f.write_str("GREATER THAN"),
            Op::LesserThan => f.write_str("LESSER THAN"),
            Op::GreaterEqual => f.write_str("GREATER THAN OR EQUAL"),
            Op::LesserEqual => f.write_str("LESSER THAN OR EQUAL"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Keyword(String),
    Ctrl(String),
    Operator(Op),
    Value(Literal),
    Indent,
    Dedent,
    Identifier(String),
    // Type(String),
    Eof,
    Newline,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Value(v) => f.write_fmt(format_args!("VALUE {}", v)),
            TokenType::Operator(o) => f.write_fmt(format_args!("OP {}", o)),
            TokenType::Ctrl(c) => f.write_fmt(format_args!("CTRL {}", c)),
            TokenType::Keyword(k) => f.write_fmt(format_args!("KEYWORD {}", k)),
            TokenType::Indent => f.write_str("INDENT {}"),
            TokenType::Dedent => f.write_str("DEDENT {}"),
            TokenType::Identifier(i) => f.write_fmt(format_args!("IDENTIFIER {}", i)),
            // TokenType::Type(t) => f.write_fmt(format_args!("TYPE {}", t)),
            TokenType::Eof => f.write_str("END OF FILE"),
            TokenType::Newline => f.write_str("NEWLINE"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token: TokenType,
    pub span: SourceSpan,
}

impl Token {
    pub fn new(token_type: TokenType, span: SourceSpan) -> Self {
        Self {
            token: token_type,
            span,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}: {:?}", self.token, self.span))
    }
}
