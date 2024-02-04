use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::syntax::*;
use crate::token::{Literal as TLiteral, Op, Token, TokenType as TT};

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("Generic error thrown")]
    #[diagnostic(code(parser::generic_error))]
    GenericError {
        message: String,
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub indentation: Vec<usize>,
}

trait MatchToken {
    fn matches(&self, token: &Token) -> bool;
}

impl MatchToken for TT {
    fn matches(&self, token: &Token) -> bool {
        &token.token == self
    }
}

impl<F> MatchToken for F
where
    F: Fn(&Token) -> bool,
{
    fn matches(&self, token: &Token) -> bool {
        self(token)
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            indentation: vec![0],
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParseError>> {
        let mut statements = vec![];
        let mut errors = vec![];

        while !self.end_of_program() {
            match self.declaration() {
                Ok(s) => statements.push(s),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    // declarations

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        self.statement()
    }

    // statements

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.matches(&[TT::Newline]) {
            self.block()
        } else if self.matches(&[TT::Keyword("if".into())]) {
            self.if_statement()
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Result<Stmt, ParseError> {
        self.consume(
            &TT::Indent,
            "Expected indentation at start of statement block",
        )?;

        let mut scope = vec![];

        while !self.check(&TT::Dedent) && !self.end_of_program() {
            scope.push(self.declaration()?);
        }
        self.consume(&TT::Dedent, "Expected dedentation after statement block")?;

        Ok(Stmt::Block(Block { scope }))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        let condition = Box::new(self.expression()?);

        self.consume(
            &TT::Ctrl(":".into()),
            "Expected ':' at end of if conditional",
        )?;
        let then_branch = Box::new(self.statement()?);

        let mut elif_condition = vec![];
        let mut elif_branch = vec![];

        if self.matches(&[TT::Keyword("elif".into())]) {
            loop {
                elif_condition.push(Some(self.expression()?));
                self.consume(
                    &TT::Ctrl(":".into()),
                    "Expected ':' at end of elif conditional",
                )?;
                elif_branch.push(Some(self.statement()?));

                if !self.matches(&[TT::Keyword("elif".into())]) {
                    break;
                }
            }
        } else {
            elif_condition.push(None);
            elif_branch.push(None);
        }

        let mut else_branch = None;

        if self.matches(&[TT::Keyword("then".into())]) {
            else_branch = Some(Box::new(self.statement()?));
        }

        self.consume(&TT::Newline, "expected newline after if block")?;

        Ok(Stmt::If(If {
            condition,
            then_branch,
            elif_condition,
            elif_branch,
            else_branch,
        }))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(&TT::Newline, "Expected newline")?;
        Ok(Stmt::Expression(Expression { expr }))
    }

    // expressions

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.cexpr()
    }

    /// handles:
    /// ID
    /// literal
    /// [lists]
    /// ( expr )
    /// member_expr : cexpr . ID
    ///             : cexpr . ID ( params )
    /// index_expr  : cexpr [ expr ]
    /// ID ( params )
    /// cexpr operator cexpr
    /// - cexpri
    fn cexpr(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.or()?;

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;

        while self.matches(&[TT::Keyword("or".into())]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while self.matches(&[TT::Keyword("and".into())]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while self.matches(&[
            TT::Operator(Op::EqualEquals),
            TT::Operator(Op::NotEquals),
            TT::Keyword("is".into()),
        ]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while self.matches(&[
            TT::Operator(Op::LesserEqual),
            TT::Operator(Op::GreaterEqual),
            TT::Operator(Op::GreaterThan),
            TT::Operator(Op::LesserThan),
        ]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            })
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while self.matches(&[TT::Operator(Op::Add), TT::Operator(Op::Subtract)]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.modulo()?;

        while self.matches(&[TT::Operator(Op::Multiply), TT::Operator(Op::Divide)]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn modulo(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while self.matches(&[TT::Operator(Op::Remainder)]) {
            let operator = self.previous();
            let right = self.modulo()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.matches(&[TT::Operator(Op::Not), TT::Operator(Op::Subtract)]) {
            let operator = self.previous();
            let right = self.unary()?;
            Ok(Expr::Unary(Unary {
                operator,
                right: Box::new(right),
            }))
        } else {
            self.index()
        }
    }

    fn index(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.call()?;

        if self.matches(&[TT::Ctrl("[".into())]) {
            let value = self.expression()?;
            self.consume(
                &TT::Ctrl("]".into()),
                "expected ']' after '[' and index value",
            )?;
            expr = Expr::Index(Index {
                value: Box::new(value),
            });
        }

        Ok(expr)
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.target()?;

        loop {
            if self.matches(&[TT::Ctrl("(".into())]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Handler for:
    /// ID
    /// ID ( params )
    /// Literal
    /// [ lists ]
    /// ( groupings )
    /// cexpr . ID
    /// cexpr . ID ( params )
    /// cexpr [ expr ]
    fn target(&mut self) -> Result<Expr, ParseError> {
        self.advance();

        match self.previous().token {
            // TT::Keyword(_) => todo!(),
            TT::Ctrl(c) => {
                if c == "(" {
                    let expr = self.expression()?;
                    self.consume(&TT::Ctrl(")".into()), "Expected ')' after '('")?;
                    Ok(Expr::Grouping(Grouping {
                        expr: Box::new(expr),
                    }))
                } else if c == "[" {
                    let mut vexpr = vec![];
                    if !self.check(&TT::Ctrl("]".into())) {
                        loop {
                            vexpr.push(self.expression()?);
                            if !self.matches(&[TT::Ctrl(",".into())]) {
                                break;
                            }
                        }
                    }

                    self.consume(&TT::Ctrl("]".into()), "expected ']' after '[")?;

                    Ok(Expr::Literal(Literal::List(vexpr)))
                } else {
                    Err(ParseError::GenericError {
                        message: "unexpected token".into(),
                        help: "idk".into(),
                        span: self.previous().span,
                    })
                }
            }
            // TT::Operator(_) => todo!(),
            TT::Value(v) => Ok(match v {
                TLiteral::Num(n) => Expr::Literal(Literal::Number(n)),
                TLiteral::Str(s) => Expr::Literal(Literal::String(s)),
                TLiteral::Boolean(b) => {
                    if b {
                        Expr::Literal(Literal::True)
                    } else {
                        Expr::Literal(Literal::False)
                    }
                }
                // TLiteral::List(l) => todo!(),
                TLiteral::None => Expr::Literal(Literal::None),
                TLiteral::Empty => Expr::Literal(Literal::Empty),
            }),
            // TT::Indent => todo!(),
            // TT::Dedent => todo!(),
            TT::Identifier(_) => Ok(Expr::Variable(Variable {
                name: self.previous(),
            })),
            // TT::Eof => todo!(),
            // TT::Newline => todo!(),
            _ => Err(ParseError::GenericError {
                message: "unexpected token".into(),
                help: "idk".into(),
                span: self.previous().span,
            }),
        }
    }

    fn finish_call(&mut self, expr: Expr) -> Result<Expr, ParseError> {
        let mut args = vec![];

        if !self.check(&TT::Ctrl(")".into())) {
            loop {
                args.push(self.expression()?);
                if !self.matches(&[TT::Ctrl(",".into())]) {
                    break;
                }
            }
        }

        let paren = self.consume(&TT::Ctrl(")".into()), "Expect ')' after function call")?;

        Ok(Expr::Call(Call {
            callee: Box::new(expr),
            paren,
            args,
        }))
    }

    // helper functions
    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn advance(&mut self) -> Token {
        if !self.end_of_program() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn consume(
        &mut self,
        token_type: &impl MatchToken,
        message: &str,
    ) -> Result<Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParseError::GenericError {
                message: message.into(),
                help: "idk my bff jill".into(),
                span: self.previous().span,
            })
        }
    }

    fn check(&self, t: &impl MatchToken) -> bool {
        if self.end_of_program() {
            false
        } else {
            t.matches(&self.peek())
        }
    }

    fn matches(&mut self, tokens: &[TT]) -> bool {
        for t in tokens {
            if self.check(t) {
                self.advance();
                return true;
            }
        }
        false
    }
    /*
        fn indent(&mut self) -> Result<usize, ParseError> {
            let token = self.peek();

            match token.token {
                TT::Indent {
                    if &i > self.indentation.last().unwrap() {
                        self.consume(&TT::Indent(i), "expecting indentation".into());
                        Ok(i)
                    } else {
                        Err(ParseError::GenericError {
                            message: "expected increased indentation".into(),
                            help: "indent the thing".into(),
                            span: token.span,
                        })
                    }
                }
                _ => Err(ParseError::GenericError {
                    message: "expected indentation".into(),
                    help: "consider indenting".into(),
                    span: token.span,
                }),
            }
        }
    */
    fn end_of_program(&self) -> bool {
        self.peek().token == TT::Eof
    }
}
