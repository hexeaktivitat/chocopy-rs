use std::collections::VecDeque;

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
            self.if_statement()
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        let condition = Box::new(self.expression()?);

        self.consume(
            &TT::Ctrl(":".into()),
            "Expected ':' at end of if conditional".into(),
        )?;
        self.consume(&TT::Newline, "Expected new line after if statement".into())?;
        let indent = self.indent()?;
        self.indentation.push(indent);
        let then_branch = Box::new(self.statement()?);

        let mut elif_condition = vec![];
        let mut elif_branch = vec![];

        if self.matches(&[TT::Keyword("elif".into())]) {
            loop {
                elif_condition.push(Some(self.expression()?));
                self.consume(
                    &TT::Ctrl(":".into()),
                    "Expected ':' at end of elif conditional".into(),
                )?;
                self.consume(
                    &TT::Newline,
                    "Expected new line after elif statement".into(),
                )?;
                let indent = self.indent()?;
                self.indentation.push(indent);
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
            self.consume(
                &TT::Newline,
                "expected new line after else statement".into(),
            )?;
            let indent = self.indent()?;
            self.indentation.push(indent);
            else_branch = Some(Box::new(self.statement()?));
        }

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
            TT::Ctrl(c) => match c.as_str() {
                "(" => {
                    let expr = self.expression()?;
                    self.consume(&TT::Ctrl(")".into()), "Expected ')' after '('".into())?;
                    Ok(Expr::Grouping(Grouping {
                        expr: Box::new(expr),
                    }))
                }

                // list handling should be moved to stmts (will need to cover typecasting)
                // typecasts could be handled here though....
                "[" => {
                    let mut list = vec![];

                    while self.peek().token != TT::Ctrl("]".into()) && !self.end_of_program() {
                        list.push(self.expression()?);
                    }

                    self.consume(&TT::Ctrl("]".into()), "Expected ']' after '['".into())?;
                    Ok(Expr::Literal(Literal::List(list)))
                }
                _ => Err(ParseError::GenericError {
                    message: "unknown errors".into(),
                    help: "idk my bff jill".into(),
                    span: self.previous().span,
                }),
            },

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
                    span: self.previous().span,
                }),
            },
            // TT::Indent(_) => todo!(), // scope indicator
            // TT::Dedent(_) => todo!(), // scope delineator
            TT::Identifier(_) => Ok(Expr::Variable(Variable {
                name: self.previous(),
            })),
            TT::Type(_) => Ok(Expr::Type(Type {
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

    fn indent(&mut self) -> Result<usize, ParseError> {
        let token = self.peek();

        match token.token {
            TT::Indent(i) => {
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

    fn end_of_program(&self) -> bool {
        self.peek().token == TT::Eof
    }
}
