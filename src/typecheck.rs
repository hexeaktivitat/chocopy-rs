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
                Err(_) => todo!(),
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt, env: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let res = walk_stmt(&mut *self, &stmt, env)?;

        Ok(res)
    }

    fn check_expr(&mut self, expr: &Expr, env: Option<Vec<String>>) -> Result<Expr, TypeError> {
        let res = walk_expr(&mut *self, &expr, env)?;

        Ok(res)
    }

    fn get_expr_type(&self, expr: &Expr) -> Option<Typed> {
        match expr {
            Expr::Literal(_, t) => t.clone(),
            Expr::Unary(u) => u.typed.clone(),
            Expr::Binary(b) => b.typed.clone(),
            Expr::Identifier(i) => Some(Typed::Inferred(Type::None)),
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
    fn visit_var(&mut self, x: &Var, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.type_id = self.check_expr(&x.type_id, None)?;
        let assigned_type = self.get_expr_type(&res.type_id);

        let initializer = x.initializer.clone();
        let mut initializer_type = Some(Typed::Inferred(Type::None));

        if let Some(init) = initializer {
            res.initializer = Some(Box::new(self.check_expr(&init, None)?));
            initializer_type = self.get_expr_type(&res.initializer.clone().unwrap());
        }

        res.typed = assigned_type;

        Ok(Stmt::Var(res))
    }

    fn visit_func(&mut self, x: &Func, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        let mut assigned_type: Option<Typed> = None;

        if let Some(id) = &x.type_id {
            res.type_id = Some(self.check_expr(&id, None)?);
            assigned_type = self.get_expr_type(&res.type_id.clone().unwrap());
        }

        let block = self.check_stmt(&x.body, None)?;
        let block_type = self.get_stmt_type(&block);

        res.typed = assigned_type;

        Ok(Stmt::Func(res))
    }

    fn visit_expression(
        &mut self,
        x: &Expression,
        state: Option<Vec<String>>,
    ) -> Result<Stmt, TypeError> {
        let expr_res = self.check_expr(&x.expr, None)?;

        let mut res = x.clone();

        res.typed = self.get_expr_type(&expr_res);
        Ok(Stmt::Expression(res))
    }

    fn visit_block(&mut self, x: &Block, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Stmt::Block(res))
    }

    fn visit_if(&mut self, x: &If, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Stmt::If(res))
    }

    fn visit_return(&mut self, x: &Return, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Stmt::Return(res))
    }

    fn visit_print(&mut self, x: &Print, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Stmt::Print(res))
    }
}

impl ExprVisitor<Result<Expr, TypeError>, Option<Vec<String>>> for &mut TypeChecker {
    fn visit_literal(
        &mut self,
        x: &Literal,
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

    fn visit_unary(&mut self, x: &Unary, state: Option<Vec<String>>) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Unary(res))
    }

    fn visit_binary(&mut self, x: &Binary, state: Option<Vec<String>>) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Binary(res))
    }

    fn visit_identifier(
        &mut self,
        x: &Identifier,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        // res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Identifier(res))
    }

    fn visit_variable(
        &mut self,
        x: &Variable,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Variable(res))
    }

    fn visit_assign(&mut self, x: &Assign, state: Option<Vec<String>>) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Assign(res))
    }

    fn visit_logical(
        &mut self,
        x: &Logical,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Logical(res))
    }

    fn visit_grouping(
        &mut self,
        x: &Grouping,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Grouping(res))
    }

    fn visit_call(&mut self, x: &Call, state: Option<Vec<String>>) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Call(res))
    }

    fn visit_index(&mut self, x: &Index, state: Option<Vec<String>>) -> Result<Expr, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::Empty));

        Ok(Expr::Index(res))
    }
}
