use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::syntax::*;
use crate::token::{Literal as TLiteral, Op, Token, TokenType as TT};

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("Unexpected token")]
    UnexpectedToken {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },

    #[error("Expected alternate token")]
    AlternateToken {
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
    pub type_search: bool,
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
            type_search: false,
        }
    }

    /// given an array of tokens, processes the token stream into an abstract syntax tree
    /// recursive descent w/o Pratt parsing
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
        let res = if self.matches(&[TT::Keyword("def".into())]) {
            self.declare_func()
        } else if matches!(self.tokens[self.current].token, TT::Identifier(_))
            && self.tokens[self.current + 1].token == TT::Ctrl(":".into())
        {
            self.declare_variable()
        } else {
            self.statement()
        };

        if res.is_err() {
            self.synchronize();
        }
        res
    }

    /// turns 'def' stmts into functions following grammar rules
    /// def ID ( [typed_var [typed_var]* ]? ) [ -> type ]? : NEWLINE INDENT func_body DEDENT
    fn declare_func(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(
            &|t: &Token| matches!(t.token, TT::Identifier(_)),
            "expected name of func after def",
        )?;

        self.consume(&TT::Ctrl("(".into()), "expected '(' after function name ")?;

        let mut params = vec![];

        if !self.check(&TT::Ctrl(")".into())) {
            loop {
                if params.len() >= 255 {
                    panic!("why");
                }

                let param_var = self.consume(
                    &|t: &Token| matches!(t.token, TT::Identifier(_)),
                    "expected variable name",
                )?;

                self.consume(&TT::Ctrl(":".into()), "expected type for param")?;

                self.type_search = true;

                let param_type = if self.peek().token == TT::Ctrl("[".into()) {
                    self.consume(&TT::Ctrl("[".into()), "expected '[' for list type")?;
                    let ty = self.consume(
                        &|t: &Token| matches!(t.token, TT::Identifier(_)),
                        "expected type declaration",
                    )?;
                    self.consume(&TT::Ctrl("]".into()), "expected ']' after '['")?;
                    ty
                } else {
                    self.consume(
                        &|t: &Token| matches!(t.token, TT::Identifier(_)),
                        "expected type declaration",
                    )?
                };

                self.type_search = false;

                let param = Stmt::Var(Var {
                    name: param_var,
                    type_id: param_type,
                    initializer: None,
                    typed: None,
                });

                params.push(param);

                if !self.matches(&[TT::Ctrl(",".into())]) {
                    break;
                }
            }
        }

        self.consume(
            &TT::Ctrl(")".into()),
            "expected ')' after function parameters",
        )?;

        // need to revisit logic here
        self.type_search = true;
        let mut type_id = None;

        if self.peek().token == TT::Ctrl("->".into()) {
            self.consume(&TT::Ctrl("->".into()), "this shouldn't fail")?;
            if self.peek().token == TT::Ctrl("[".into()) {
                self.consume(&TT::Ctrl("[".into()), "expected '[' for list")?;
                type_id = Some(self.consume(
                    &|t: &Token| matches!(t.token, TT::Identifier(_)),
                    "expected type declaration",
                )?);
                self.consume(&TT::Ctrl("]".into()), "expected ']' after '['")?;
            } else {
                type_id = Some(self.consume(
                    &|t: &Token| matches!(t.token, TT::Identifier(_)),
                    "expected type declaration",
                )?);
            }
        }

        self.type_search = false;

        self.consume(
            &TT::Ctrl(":".into()),
            "expected ':' after function definition",
        )?;

        self.consume(&TT::Newline, "expected newline after function declaration")?;

        let body = self.block()?;

        self.consume(&TT::Newline, "expected newline after function block")?;

        Ok(Stmt::Func(Func {
            name,
            type_id,
            parameters: params,
            body: Box::new(body),
            typed: None,
        }))
    }

    /// generates a variable stmt from the grammar
    /// ID : type = literal NEWLINE
    fn declare_variable(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(
            &|t: &Token| matches!(t.token, TT::Identifier(_)),
            "expected variable identifier",
        )?;

        self.consume(
            &TT::Ctrl(":".into()),
            "expected : for type declaration after var name",
        )?;

        // need to revisit logic here
        let type_id = if self.peek().token == TT::Ctrl("[".into()) {
            self.consume(&TT::Ctrl("[".into()), "expected '[' after list decl")?;
            let ty = self.consume(
                &|t: &Token| matches!(t.token, TT::Identifier(_)),
                "expected list identifier",
            )?;
            self.consume(&TT::Ctrl("]".into()), "expected ']' after list")?;
            ty
        } else {
            self.consume(
                &|t: &Token| matches!(t.token, TT::Identifier(_)),
                "expected type after declaration",
            )?
        };

        let mut initializer = None;
        if self.matches(&[TT::Operator(Op::Equals)]) {
            initializer = Some(Box::new(self.expression()?));
        }

        self.consume(&TT::Newline, "expected newline after variable decl")?;

        Ok(Stmt::Var(Var {
            name,
            type_id,
            initializer,
            typed: None,
        }))
    }

    // statements

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let res = if self.matches(&[TT::Indent]) {
            self.block()
        } else if self.matches(&[TT::Keyword("if".into())]) {
            self.if_statement()
        } else if self.matches(&[TT::Keyword("return".into())]) {
            self.return_statement()
        } else {
            self.expression_statement()
        };
        res
    }

    /// processes an indented block of stmts
    /// NEWLINE INDENT stmt+ DEDENT
    /// assumes the NEWLINE has been consumed prior!!
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

        Ok(Stmt::Block(Block { scope, typed: None }))
    }

    /// processes an if stmt
    /// if expr : block [elif expr : block ]* [else : block]
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
            typed: None,
        }))
    }

    /// return values from function blocks
    /// return [expr]?
    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let keyword = self.previous().clone();
        let value = if self.check(&TT::Newline) {
            self.consume(&TT::Newline, "expected newline after return statement")?;
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(&TT::Newline, "expected newline after return statement")?;

        Ok(Stmt::Return(Return {
            keyword,
            value: value.map(Box::new),
            typed: None,
        }))
    }

    /// returns an expr as a statement
    ///
    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(&TT::Newline, "Expected newline after expr")?;
        Ok(Stmt::Expression(Expression { expr, typed: None }))
    }

    // expressions

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    /// handles assignment expressions
    /// [target =]+ expr
    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.cexpr()?;

        if self.matches(&[TT::Operator(Op::Equals)]) {
            let _equals = self.previous();
            let value = self.assignment()?;
            if let Expr::Variable(v) = expr {
                let name = v.name;
                Ok(Expr::Assign(Assign {
                    name,
                    value: Box::new(value),
                    typed: None,
                }))
            } else {
                Ok(expr)
            }
        } else {
            Ok(expr)
        }
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
        let expr = self.or()?;

        Ok(expr)
    }

    /// or exprs
    /// expr or expr
    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;

        while self.matches(&[TT::Keyword("or".into())]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                typed: None,
            });
        }

        Ok(expr)
    }

    /// and expr
    /// expr and expr
    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while self.matches(&[TT::Keyword("and".into())]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                typed: None,
            });
        }

        Ok(expr)
    }

    /// equality exprs
    /// expr [ == | != | is ] expr
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
                typed: None,
            });
        }

        Ok(expr)
    }

    /// comparison exprs
    /// expr [ < | > | <= | >= ] expr
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
                typed: None,
            })
        }

        Ok(expr)
    }

    /// addition and subtraction exprs
    /// expr [ + | - ] expr
    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while self.matches(&[TT::Operator(Op::Add), TT::Operator(Op::Subtract)]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                typed: None,
            });
        }

        Ok(expr)
    }

    /// multiplication and division exprs
    /// expr [ * | // ] expr
    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.modulo()?;

        while self.matches(&[TT::Operator(Op::Multiply), TT::Operator(Op::Divide)]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                typed: None,
            });
        }

        Ok(expr)
    }

    /// modulo exprs
    /// expr % expr
    fn modulo(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while self.matches(&[TT::Operator(Op::Remainder)]) {
            let operator = self.previous();
            let right = self.modulo()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                typed: None,
            });
        }

        Ok(expr)
    }

    /// negation and inversion
    /// [ not | - ] expr
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.matches(&[TT::Operator(Op::Not), TT::Operator(Op::Subtract)]) {
            let operator = self.previous();
            let right = self.unary()?;
            Ok(Expr::Unary(Unary {
                operator,
                right: Box::new(right),
                typed: None,
            }))
        } else {
            self.index()
        }
    }

    /// index exprs
    /// [ expr ]
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
                typed: None,
            });
        }

        Ok(expr)
    }

    /// function invocation exprs
    /// ID ( )
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
                        typed: None,
                    }))
                } else if c == "[" {
                    self.make_list()
                } else {
                    Err(ParseError::UnexpectedToken {
                        help: "tried to match '(' or '[' and failed".into(),
                        span: self.tokens[self.current].span,
                    })
                }
            }
            // TT::Operator(_) => todo!(),
            TT::Value(v) => Ok(match v {
                TLiteral::Num(n) => Expr::Literal(Literal::Number(n), None),
                TLiteral::Str(s) => Expr::Literal(Literal::String(s), None),
                TLiteral::Boolean(b) => {
                    if b {
                        Expr::Literal(Literal::True, None)
                    } else {
                        Expr::Literal(Literal::False, None)
                    }
                }
                TLiteral::None => Expr::Literal(Literal::None, None),
                TLiteral::Empty => Expr::Literal(Literal::Empty, None),
            }),
            // TT::Indent => todo!(),
            // TT::Dedent => todo!(),
            TT::Identifier(_) => {
                if self.type_search {
                    Ok(Expr::Identifier(Identifier {
                        name: self.previous(),
                        typed: None,
                    }))
                } else {
                    Ok(Expr::Variable(Variable {
                        name: self.previous(),
                        typed: None,
                    }))
                }
            }
            // TT::Eof => todo!(),
            _ => Err(ParseError::UnexpectedToken {
                help: format!(
                    "terminal parse resulted in null token. \nprev: {:?}\ncur: {:?}\nnext: {:?}",
                    self.previous(),
                    self.peek(),
                    self.tokens[self.current + 1]
                ),
                span: self.tokens[self.current].span,
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
            typed: None,
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
            Err(ParseError::AlternateToken {
                help: message.into(),
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

    fn make_list(&mut self) -> Result<Expr, ParseError> {
        let mut vexpr = vec![];
        if !self.check(&TT::Ctrl("]".into())) {
            loop {
                vexpr.push(self.expression()?);
                if !self.matches(&[TT::Ctrl(",".into())]) {
                    break;
                }
            }
        } else {
            vexpr.push(Expr::Literal(Literal::Empty, None));
        }

        self.consume(&TT::Ctrl("]".into()), "expected ']' after '[")?;

        Ok(Expr::Literal(Literal::List(vexpr), None))
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

    fn synchronize(&mut self) {
        self.advance();

        while !self.end_of_program() {
            if self.previous().token == TT::Newline {
                return;
            } else {
                match self.peek().token {
                    TT::Keyword(_) => return,
                    _ => self.advance(),
                };
            }
        }
    }
}
