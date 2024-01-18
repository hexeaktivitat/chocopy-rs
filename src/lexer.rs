use std::num::Saturating;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::token::*;

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic(code(oops::my::bad), url(docsrs), help("do it better next time"))]
struct LexError {
    #[source_code]
    src: String,
    span: SourceSpan,
    msg: String,
}

struct Lexer<'a> {
    source: &'a [u8],
    tokens: Vec<Token>,
    // syntax_errors: Vec<Error>,
    start: usize,
    current: usize,
    line: usize,
    indent: Vec<usize>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            // syntax_errors: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            indent: vec![0],
        }
    }

    // process individual character input and generate tokens
    fn scan_token(&mut self) -> Result<Option<TokenType>, LexError> {
        // current char location in the source code (for spans)
        self.start = self.current;

        match self.advance() {
            // single characters to start with
            // control characters
            b'(' => Ok(Some(TokenType::Ctrl('('))),
            b')' => Ok(Some(TokenType::Ctrl(')'))),
            b'{' => Ok(Some(TokenType::Ctrl('{'))),
            b'}' => Ok(Some(TokenType::Ctrl('}'))),
            b'[' => Ok(Some(TokenType::Ctrl('['))),
            b']' => Ok(Some(TokenType::Ctrl(']'))),
            b':' => Ok(Some(TokenType::Ctrl(':'))),
            b',' => Ok(Some(TokenType::Ctrl(','))),
            b'.' => Ok(Some(TokenType::Ctrl('.'))),
            b'#' => Ok(Some(TokenType::Ctrl('#'))),
            // significant whitespace
            // space handling for indent / dedent may be better to do in a different place
            // b' ' => Ok(Some(TokenType::Indent(self.indent + 1))),
            b'\r' | b' ' | b'\t' => Ok(None),
            b'\n' => {
                self.line += 1;
                let mut indentation = 0;

                // indent checking here
                while self.match_next(b' ') {
                    indentation += 1;
                }

                if &indentation > self.indent.last().unwrap() {
                    self.indent.push(indentation);
                    Ok(Some(TokenType::Indent(indentation)))
                } else if &indentation < self.indent.last().unwrap() {
                    while &indentation < self.indent.last().unwrap() {
                        self.indent.pop();
                    }
                    Ok(Some(TokenType::Dedent(indentation)))
                } else {
                    Ok(Some(TokenType::Ctrl('\n')))
                }
            }
            // b'\t' => Ok(Some(TokenType::Ctrl('\t'))),
            // operators
            b'+' => Ok(Some(TokenType::Operator(Op::Add))),
            b'-' => Ok(Some(TokenType::Operator(Op::Subtract))),
            b'*' => Ok(Some(TokenType::Operator(Op::Multiply))),
            b'%' => Ok(Some(TokenType::Operator(Op::Remainder))),

            // potential two-character tokens
            b'/' => {
                if self.match_next(b'/') {
                    Ok(Some(TokenType::Operator(Op::Divide)))
                } else {
                    Err(LexError {
                        src: String::from_utf8(self.source[self.start..self.current].to_owned())
                            .unwrap(),
                        span: (self.start, self.current).into(),
                        msg: "Syntax error: '/'".into(),
                    })
                }
            }
            b'=' => Ok(Some(if self.match_next(b'=') {
                TokenType::Operator(Op::EqualEquals)
            } else {
                TokenType::Operator(Op::Equals)
            })),
            b'!' => Ok(Some(if self.match_next(b'=') {
                TokenType::Operator(Op::NotEquals)
            } else {
                TokenType::Operator(Op::Not)
            })),
            b'>' => Ok(Some(if self.match_next(b'=') {
                TokenType::Operator(Op::GreaterEqual)
            } else {
                TokenType::Operator(Op::GreaterThan)
            })),
            b'<' => Ok(Some(if self.match_next(b'=') {
                TokenType::Operator(Op::LesserEqual)
            } else {
                TokenType::Operator(Op::LesserThan)
            })),
            // good enough general error handling for now
            _ => Err(LexError {
                src: String::from_utf8(self.source[self.start..self.current].to_owned()).unwrap(),
                span: (self.start, self.current).into(),
                msg: "Syntax error: unknown character".into(),
            }),
        }
    }
    // returns the next byte of source code and advances the internal counter
    fn advance(&mut self) -> u8 {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    // EOF test
    fn end_of_code(&self) -> bool {
        self.current >= self.source.len()
    }

    // lookahead(1)
    fn peek(&self) -> u8 {
        *self.source.get(self.current).unwrap_or(&b'\0')
    }

    // lookahead(2)
    fn peek_next(&self) -> u8 {
        *self.source.get(self.current + 1).unwrap_or(&b'\0')
    }

    fn match_next(&mut self, expected: u8) -> bool {
        if self.end_of_code() || self.source[self.current] != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }
}
