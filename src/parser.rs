use miette::{Diagnostic, Error, Result, SourceSpan};
use thiserror::Error;

use crate::syntax::*;
use crate::token::{Token, TokenType as TT};

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
                message: "Token consumption failed".into(),
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

    fn matches(&self, tokens: &[TT]) -> bool {
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
