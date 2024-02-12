use std::arch::x86_64::_mm256_permute2f128_ps;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::syntax::*;
use crate::token::{Token, TokenType as TT};

#[derive(Error, Debug, Diagnostic)]
pub enum TypeError {
    #[error("Invalid type")]
    InvalidType {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },

    #[error("type declaration and derived type do not match")]
    DeclMismatch {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeChecker {
    pub ast: Vec<Stmt>,
}

impl TypeChecker {
    pub fn new(ast: &Vec<Stmt>) -> Self {
        Self { ast: ast.clone() }
    }

    pub fn verify(&mut self) -> Result<Vec<Stmt>, Vec<TypeError>> {
        let mut result = vec![];
        let mut errors = vec![];

        for stmt in self.ast.clone().iter_mut() {
            match self.check_stmt(stmt, None) {
                Ok(s) => result.push(s),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    fn check_stmt(&mut self, stmt: &mut Stmt, env: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let res = walk_stmt(&mut *self, stmt, env)?;

        Ok(res)
    }

    fn check_expr(&mut self, expr: &mut Expr, env: Option<Vec<String>>) -> Result<Expr, TypeError> {
        let res = walk_expr(&mut *self, expr, env)?;

        Ok(res)
    }

    fn get_expr_type(&self, expr: &Expr) -> Option<Typed> {
        match expr {
            Expr::Literal(_, t) => t.clone(),
            Expr::Unary(u) => u.typed.clone(),
            Expr::Binary(b) => b.typed.clone(),
            Expr::Identifier(i) => Some(Typed::Inferred(Type::Id(i.name.token.to_string()))),
            Expr::Assign(a) => a.typed.clone(),
            Expr::Logical(l) => l.typed.clone(),
            Expr::Grouping(g) => g.typed.clone(),
            Expr::Call(c) => c.typed.clone(),
            Expr::Variable(v) => v.typed.clone(),
            Expr::Index(i) => i.typed.clone(),
        }
    }

    fn get_stmt_type(&self, stmt: &Stmt) -> Option<Typed> {
        match stmt {
            Stmt::Var(v) => v.typed.clone(),
            Stmt::Func(f) => f.typed.clone(),
            Stmt::Expression(e) => e.typed.clone(),
            Stmt::Block(b) => b.typed.clone(),
            Stmt::If(i) => i.typed.clone(),
            Stmt::Return(r) => r.typed.clone(),
            Stmt::Print(p) => p.typed.clone(),
        }
    }
}

impl StmtVisitor<Result<Stmt, TypeError>, Option<Vec<String>>> for &mut TypeChecker {
    fn visit_var(&mut self, x: &mut Var, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let checked_expr = self.check_expr(&mut x.type_id, None)?;
        let assigned_type = self.get_expr_type(&checked_expr);

        let mut initializer_type = Some(Typed::Inferred(Type::None));

        if let Some(init) = &mut x.initializer.clone() {
            x.initializer = Some(Box::new(self.check_expr(init, None)?));
            initializer_type = self.get_expr_type(x.initializer.as_ref().unwrap());
        }

        if !(assigned_type == initializer_type) {
            Err(TypeError::DeclMismatch {
                help: format!(
                    "assigned type was {:#?} while initialized type was {:#?}",
                    assigned_type, initializer_type
                ),
                span: x.name.span,
            })
        } else {
            let res = x.clone();
            Ok(Stmt::Var(res))
        }
    }

    fn visit_func(&mut self, x: &mut Func, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut assigned_type: Option<Typed> = None;

        if let Some(id) = &mut x.type_id {
            x.type_id = Some(self.check_expr(id, None)?);
            assigned_type = self.get_expr_type(&x.type_id.as_ref().unwrap());
        }

        let block = self.check_stmt(&mut x.body, None)?;
        let block_type = self.get_stmt_type(&block);

        if !(assigned_type == block_type) {
            Err(TypeError::DeclMismatch {
                help: format!(
                    "assigned type was {:#?} while initialized type was {:#?}",
                    assigned_type, block_type
                ),
                span: x.name.span,
            })
        } else {
            let res = x.clone();
            Ok(Stmt::Func(res))
        }
    }

    fn visit_expression(
        &mut self,
        x: &mut Expression,
        state: Option<Vec<String>>,
    ) -> Result<Stmt, TypeError> {
        let expr_res = self.check_expr(&mut x.expr, None)?;

        let mut res = x.clone();

        res.typed = self.get_expr_type(&expr_res);
        Ok(Stmt::Expression(res))
    }

    fn visit_block(
        &mut self,
        x: &mut Block,
        state: Option<Vec<String>>,
    ) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Stmt::Block(res))
    }

    fn visit_if(&mut self, x: &mut If, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Stmt::If(res))
    }

    fn visit_return(
        &mut self,
        x: &mut Return,
        state: Option<Vec<String>>,
    ) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Stmt::Return(res))
    }

    fn visit_print(
        &mut self,
        x: &mut Print,
        state: Option<Vec<String>>,
    ) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Stmt::Print(res))
    }
}

impl ExprVisitor<Result<Expr, TypeError>, Option<Vec<String>>> for &mut TypeChecker {
    fn visit_literal(
        &mut self,
        x: &mut Literal,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        let typed = match x {
            Literal::Number(_) => Type::Num,
            Literal::String(_) => Type::Str,
            Literal::List(_) => Type::List(Box::new(Type::Empty)),
            Literal::True => Type::Bool,
            Literal::False => Type::Bool,
            Literal::None => Type::None,
            Literal::Empty => Type::Empty,
            Literal::Eol => Type::None,
        };

        Ok(Expr::Literal(res, Some(Typed::Assigned(typed))))
    }

    fn visit_unary(
        &mut self,
        x: &mut Unary,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Unary(res))
    }

    fn visit_binary(
        &mut self,
        x: &mut Binary,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Binary(res))
    }

    fn visit_identifier(
        &mut self,
        x: &mut Identifier,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        // res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Identifier(res))
    }

    fn visit_variable(
        &mut self,
        x: &mut Variable,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Variable(res))
    }

    fn visit_assign(
        &mut self,
        x: &mut Assign,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Assign(res))
    }

    fn visit_logical(
        &mut self,
        x: &mut Logical,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Logical(res))
    }

    fn visit_grouping(
        &mut self,
        x: &mut Grouping,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Grouping(res))
    }

    fn visit_call(&mut self, x: &mut Call, state: Option<Vec<String>>) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Call(res))
    }

    fn visit_index(
        &mut self,
        x: &mut Index,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Index(res))
    }
}
