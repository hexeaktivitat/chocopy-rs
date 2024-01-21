use miette::{Diagnostic, Error, NamedSource, Result, SourceSpan};
use thiserror::Error;

use crate::syntax::*;
use crate::token::{Literal as TLiteral, Token, TokenType as TT};

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

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
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
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParseError>> {
        let mut statements = vec![];
        let mut errors = vec![];

        while !self.end_of_program() {
            match self.statement() {
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

    // statements

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.matches(&[TT::Keyword("if".into())]) {
            todo!()
        } else {
            self.expression_statement()
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(&TT::Newline, "Expected newline".into())?;
        Ok(Stmt::Expression(Expression { expr }))
    }

    // expressions

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.or();

        expr
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let expr = self.and();

        expr
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let expr = self.equality();

        expr
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let expr = self.comparison();

        expr
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let expr = self.term();

        expr
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let expr = self.factor();

        expr
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let expr = self.unary();

        expr
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        let expr = self.call();

        expr
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let expr = self.primary();

        expr
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        self.advance();

        match self.previous().token {
            // TT::Keyword(_) => todo!(),
            TT::Ctrl(c) => {
                if c == "(" {
                    let expr = self.expression()?;
                    self.consume(&TT::Ctrl(")".into()), "Expected ')' after '('".into())?;
                    Ok(Expr::Grouping(Grouping {
                        expr: Box::new(expr),
                    }))
                } else if c == "[" {
                    let mut list = vec![];

                    while self.peek().token != TT::Ctrl("]".into()) && !self.end_of_program() {
                        list.push(self.expression()?);
                    }

                    self.consume(&TT::Ctrl("]".into()), "Expected ']' after '['".into())?;
                    Ok(Expr::Literal(Literal::List(list)))
                } else {
                    Err(ParseError::GenericError {
                        message: "unknown errors".into(),
                        help: "idk my bff jill".into(),
                        span: self.tokens[self.current].span,
                    })
                }
            }
            // TT::Operator(_) => todo!(),
            TT::Value(literal) => match literal {
                TLiteral::Num(n) => Ok(Expr::Literal(Literal::Number(n))),
                TLiteral::Str(s) => Ok(Expr::Literal(Literal::String(s))),
                TLiteral::Boolean(b) => Ok(if b {
                    Expr::Literal(Literal::True)
                } else {
                    Expr::Literal(Literal::False)
                }),
                // TLiteral::List(_) => todo!(), // this one tricky???
                // opting to handle Lists from Ctrl("[")
                TLiteral::None => Ok(Expr::Literal(Literal::None)),
                TLiteral::Empty => Ok(Expr::Literal(Literal::Empty)),
                _ => Err(ParseError::GenericError {
                    message: "invalid literal".into(),
                    help: "idk my bff jill".into(),
                    span: self.tokens[self.current].span,
                }),
            },
            // TT::Indent(_) => todo!(), // scope indicator
            // TT::Dedent(_) => todo!(), // scope delineator
            TT::Identifier(_) => Ok(Expr::Variable(Variable {
                name: self.previous(),
            })),
            // TT::Eof => todo!(),
            // TT::Newline => todo!(),
            _ => Ok(Expr::Literal(Literal::Eol)),
        }
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
        message: String,
    ) -> Result<Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(ParseError::GenericError {
                message,
                help: "idk my bff jill".into(),
                span: self.tokens[self.current].span,
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

    fn end_of_program(&self) -> bool {
        self.peek().token == TT::Eof
    }
}
