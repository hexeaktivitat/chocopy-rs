use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::token::*;

#[derive(Error, Debug, Diagnostic)]
pub enum LexError {
    #[error("Syntax error: Invalid character")]
    #[diagnostic(code(syntax::invalid_character))]
    InvalidCharacter {
        #[help]
        advice: String,
        #[label]
        bad_char: SourceSpan,
    },
    #[error("Syntax error: Unterminated string")]
    #[diagnostic(code(syntax::unterminated_string))]
    UnterminatedString {
        #[help]
        advice: String,
        #[label]
        start_quote: SourceSpan,
    },
    #[error("Syntax error: Not a valid number")]
    #[diagnostic(code(syntax::not_a_number))]
    NotANumber {
        #[help]
        advice: String,
        #[label]
        bad_num: SourceSpan,
    },
}

pub struct Lexer<'a> {
    source: &'a [u8],
    // tokens: Vec<Token>,
    // syntax_errors: Vec<Error>,
    start: usize,
    current: usize,
    line: usize,
    // line_start: usize,
    indent: Vec<usize>,
    indent_flag: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self {
            source,
            // tokens: Vec::new(),
            // syntax_errors: Vec::new(),
            start: 0,
            current: 0,
            line: 0,
            // line_start: 0,
            indent: vec![0],
            indent_flag: false,
        }
    }

    pub fn lex_code(&mut self) -> Result<Vec<Token>, Vec<LexError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while !self.end_of_code() {
            match self
                .scan_token()
                .and_then(|ot| ot.map(|t| self.make_token(t)).transpose())
            {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => {}
                Err(e) => errors.push(e),
            }
        }

        tokens.push(Token::new(
            TokenType::Eof,
            (self.start, self.current - self.start).into(),
        ));

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    // process individual character input and generate tokens
    fn scan_token(&mut self) -> Result<Option<TokenType>, LexError> {
        // current char location in the source code (for spans)
        self.start = self.current;

        if self.indent_flag {
            self.indent_flag = false;
            let mut indentation = 0;

            while self.match_next(b' ') {
                indentation += 1;
            }

            if &indentation > self.indent.last().unwrap() {
                self.indent.push(indentation);
                return Ok(Some(TokenType::Indent));
            } else if &indentation < self.indent.last().unwrap() {
                while &indentation < self.indent.last().unwrap() {
                    self.indent.pop();
                }
                return Ok(Some(TokenType::Dedent));
            } else {
                return Ok(None);
            }
        }

        match self.advance() {
            // single characters to start with
            // control characters
            b'(' => Ok(Some(TokenType::Ctrl("(".into()))),
            b')' => Ok(Some(TokenType::Ctrl(")".into()))),
            b'{' => Ok(Some(TokenType::Ctrl("{".into()))),
            b'}' => Ok(Some(TokenType::Ctrl("}".into()))),
            b'[' => Ok(Some(TokenType::Ctrl("[".into()))),
            b']' => Ok(Some(TokenType::Ctrl("]".into()))),

            // check for types here?
            b':' => Ok(Some(TokenType::Ctrl(":".into()))),
            b',' => Ok(Some(TokenType::Ctrl(",".into()))),
            b'.' => Ok(Some(TokenType::Ctrl(".".into()))),
            b'#' => {
                while self.peek() != b'\n' && !self.end_of_code() {
                    self.advance();
                }
                // self.line += 1;
                // self.indent_flag = true;
                Ok(None)
            }

            // significant whitespace
            // space handling for indent / dedent may be better to do in a different place
            // b' ' => Ok(Some(TokenType::Indent(self.indent + 1))),
            b' ' | b'\t' | b'\r' => Ok(None),
            b'\n' => {
                self.line += 1;
                self.indent_flag = true;
                Ok(Some(TokenType::Newline))
            }
            // b'\t' => Ok(Some(TokenType::Ctrl('\t'))),

            // operators
            b'+' => Ok(Some(TokenType::Operator(Op::Add))),
            b'*' => Ok(Some(TokenType::Operator(Op::Multiply))),
            b'%' => Ok(Some(TokenType::Operator(Op::Remainder))),

            // potential two-character tokens
            b'/' => {
                if self.match_next(b'/') {
                    Ok(Some(TokenType::Operator(Op::Divide)))
                } else {
                    Err(LexError::InvalidCharacter {
                        advice: "this character is invalid in this context".into(),
                        bad_char: (self.start, self.current - self.start).into(),
                    })
                }
            }
            b'-' => {
                if self.match_next(b'>') {
                    Ok(Some(TokenType::Ctrl("->".into())))
                } else {
                    Ok(Some(TokenType::Operator(Op::Subtract)))
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

            // string literals
            b'"' => self.string().map(Some),
            // numeric literals
            c if c.is_ascii_digit() => self.number().map(Some),

            // keywords & identifiers
            c @ b'_' | c if c.is_ascii_alphabetic() => {
                self.identifier().map(|x| match x.as_str() {
                    // keywords first
                    "and" | "as" | "assert" | "async" | "await" | "break" | "class"
                    | "continue" | "def" | "del" | "elif" | "else" | "except" | "finally"
                    | "for" | "from" | "global" | "if" | "import" | "in" | "is" | "lambda"
                    | "nonlocal" | "not" | "or" | "pass" | "raise" | "return" | "try" | "while"
                    | "with" | "yield" => Some(TokenType::Keyword(x)),

                    "False" => Some(TokenType::Value(Literal::Boolean(false))),
                    "True" => Some(TokenType::Value(Literal::Boolean(true))),
                    "None" => Some(TokenType::Value(Literal::None)),
                    "Empty" => Some(TokenType::Value(Literal::Empty)),

                    // identifier for function or variable or type
                    _ => Some(TokenType::Identifier(x)),
                })
            }

            // good enough general error handling for now
            _ => Err(LexError::InvalidCharacter {
                advice: "this character is unknown to the grammar".into(),
                bad_char: (self.start, self.current - self.start).into(),
            }),
        }
    }

    fn make_token(&self, token_type: TokenType) -> Result<Token, LexError> {
        let lexeme = (self.start, self.current - self.start).into();
        Ok(Token::new(token_type, lexeme))
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

    fn string(&mut self) -> Result<TokenType, LexError> {
        while self.peek() != b'"' && !self.end_of_code() {
            if self.peek() == b'\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.end_of_code() {
            return Err(LexError::UnterminatedString {
                advice: "check for a missing quote around intended string".into(),
                start_quote: (self.start, self.current - self.start).into(),
            });
        }

        self.advance();

        Ok(TokenType::Value(Literal::Str(
            self.substring(self.start + 1, self.current - 1)?,
        )))
    }

    fn substring(&self, start: usize, end: usize) -> Result<String, LexError> {
        String::from_utf8(self.source[start..end].to_vec()).map_err(|_source| {
            LexError::InvalidCharacter {
                advice: "character unrecognized in this context".into(),
                bad_char: (self.start, self.current - self.start).into(),
            }
        })
    }

    fn number(&mut self) -> Result<TokenType, LexError> {
        while self.peek().is_ascii_digit() && !self.end_of_code() && self.peek() != b'.' {
            self.advance();
        }

        if self.end_of_code() {
            return Err(LexError::NotANumber {
                advice: "not a valid integer value".into(),
                bad_num: (self.start, self.current - self.start).into(),
            });
        } else if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            return Err(LexError::InvalidCharacter {
                advice: "chocopy-rs does not support float values".into(),
                bad_char: (self.current, self.current - self.start).into(),
            });
        } else if self.peek().is_ascii_alphabetic() {
            return Err(LexError::NotANumber {
                advice: "not a valid integer value".into(),
                bad_num: (self.start, self.current - self.start).into(),
            });
        }

        Ok(TokenType::Value(Literal::Num(
            self.substring(self.start, self.current)?
                .parse::<i32>()
                .expect("that was not an i32? how"),
        )))
    }

    fn identifier(&mut self) -> Result<String, LexError> {
        while self.peek().is_ascii_alphabetic() || self.peek() == b'_' {
            self.advance();
        }

        self.substring(self.start, self.current)
    }
}
