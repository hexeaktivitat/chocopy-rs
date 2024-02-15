use std::collections::HashMap;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::syntax::*;
use crate::token::{Op, Token, TokenType as TT};

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
    #[error("types do not match")]
    TypeMismatch {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeChecker {
    pub ast: Vec<Stmt>,
    pub symbols: HashMap<String, Typed>,
}

impl TypeChecker {
    pub fn new(ast: &Vec<Stmt>) -> Self {
        Self {
            ast: ast.clone(),
            symbols: HashMap::new(),
        }
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

    fn get_identifier_type(&self, id: &Token) -> Result<Option<Typed>, TypeError> {
        match &id.token {
            TT::Identifier(i) => {
                if i == "int" {
                    Ok(Some(Typed::Assigned(Type::Num)))
                } else if i == "str" {
                    Ok(Some(Typed::Assigned(Type::Str)))
                } else if i == "bool" {
                    Ok(Some(Typed::Assigned(Type::Bool)))
                } else if i == "None" {
                    Ok(Some(Typed::Assigned(Type::None)))
                } else {
                    Ok(Some(Typed::Assigned(Type::Id(i.clone()))))
                }
            }
            _ => unreachable!(),
        }
    }

    fn match_type(&self, a: &Typed, b: &Typed) -> bool {
        let a_type = match a {
            Typed::Assigned(t) => t,
            Typed::Inferred(t) => t,
        };

        let b_type = match b {
            Typed::Assigned(t) => t,
            Typed::Inferred(t) => t,
        };

        a_type == b_type
    }
}

impl StmtVisitor<Result<Stmt, TypeError>, Option<Vec<String>>> for &mut TypeChecker {
    fn visit_var(&mut self, x: &mut Var, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let assigned_type = self.get_identifier_type(&x.type_id)?;
        let var_name = match &x.name.token {
            TT::Identifier(i) => i.clone(),
            _ => unreachable!(),
        };

        if x.initializer.is_some() {
            let mut initializer_type = Some(Typed::Inferred(Type::None));

            if let Some(init) = &mut x.initializer {
                x.initializer = Some(Box::new(self.check_expr(init, None)?));
                initializer_type = self.get_expr_type(x.initializer.as_ref().unwrap());
            }

            if !self.match_type(
                &assigned_type.clone().unwrap(),
                &initializer_type.clone().unwrap(),
            ) && (initializer_type != Some(Typed::Assigned(Type::None)))
                && (initializer_type != Some(Typed::Assigned(Type::Empty)))
            {
                Err(TypeError::DeclMismatch {
                    help: format!(
                        "assigned type was {:#?} while initialized type was {:#?}",
                        assigned_type, initializer_type
                    ),
                    span: x.name.span,
                })
            } else {
                x.typed = assigned_type;
                if !self.symbols.contains_key(&var_name.clone()) {
                    self.symbols.insert(var_name, x.typed.clone().unwrap());
                }
                let res = x.clone();
                Ok(Stmt::Var(res))
            }
        } else {
            x.typed = assigned_type;
            self.symbols.insert(var_name, x.typed.clone().unwrap());
            let res = x.clone();
            Ok(Stmt::Var(res))
        }
    }

    fn visit_func(&mut self, x: &mut Func, state: Option<Vec<String>>) -> Result<Stmt, TypeError> {
        let mut assigned_type: Option<Typed> = None;

        let func_name = match &x.name.token {
            TT::Identifier(i) => i.clone(),
            _ => unreachable!(),
        };

        if let Some(id) = &mut x.type_id.clone() {
            assigned_type = self.get_identifier_type(&id)?;
        }

        let mut param_types = vec![];
        let mut params = vec![];

        for p in x.parameters.iter_mut() {
            params.push(self.check_stmt(p, None)?);
            let ptype = self.get_stmt_type(p);
            match p {
                Stmt::Var(v) => {
                    let p_name = match &v.name.token {
                        TT::Identifier(i) => i.clone(),
                        _ => unreachable!(),
                    };
                    self.symbols.insert(p_name, ptype.clone().unwrap());
                }
                _ => unreachable!(),
            }
            param_types.push(ptype);
        }

        let block = self.check_stmt(&mut x.body, None)?;
        let block_type = self.get_stmt_type(&block).clone().unwrap();

        if self.match_type(&assigned_type.clone().unwrap(), &block_type) {
            x.typed = assigned_type;
            self.symbols.insert(func_name, x.typed.clone().unwrap());
            let res = x.clone();
            Ok(Stmt::Func(res))
        } else {
            Err(TypeError::DeclMismatch {
                help: format!(
                    "assigned type was {:#?} while initialized type was {:#?}",
                    assigned_type, block_type
                ),
                span: x.name.span,
            })
        }
    }

    fn visit_expression(
        &mut self,
        x: &mut Expression,
        state: Option<Vec<String>>,
    ) -> Result<Stmt, TypeError> {
        let expr_res = self.check_expr(&mut x.expr, None)?;
        x.typed = self.get_expr_type(&expr_res);

        let mut res = x.clone();
        Ok(Stmt::Expression(res))
    }

    fn visit_block(
        &mut self,
        x: &mut Block,
        state: Option<Vec<String>>,
    ) -> Result<Stmt, TypeError> {
        let mut ret_types = vec![];
        for i in 0..x.scope.len() {
            x.scope[i] = self.check_stmt(&mut x.scope[i], None)?;
            if matches!(x.scope[i], Stmt::Return(_)) {
                ret_types.push(self.get_stmt_type(&x.scope[i]));
            }
        }

        if ret_types.is_empty() {
            x.typed = Some(Typed::Inferred(Type::None));
        } else {
            let mut prev = None;
            for t in ret_types {
                if prev == None {
                    prev = t;
                } else if prev == t {
                    prev = t;
                }
            }
            x.typed = prev;
        }

        let res = x.clone();
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
        x.value = if let Some(v) = &mut x.value {
            Some(Box::new(self.check_expr(v, None)?))
        } else {
            None
        };

        if x.value.is_some() {
            x.typed = self.get_expr_type(x.value.as_ref().unwrap());
        } else {
            x.typed = Some(Typed::Inferred(Type::None));
        }

        let res = x.clone();
        Ok(Stmt::Return(res))
    }

    fn visit_print(
        &mut self,
        x: &mut Print,
        state: Option<Vec<String>>,
    ) -> Result<Stmt, TypeError> {
        let mut res = x.clone();

        res.typed = Some(Typed::Assigned(Type::None));

        Ok(Stmt::Print(res))
    }
}

impl ExprVisitor<Result<Expr, TypeError>, Option<Vec<String>>> for &mut TypeChecker {
    fn visit_literal(
        &mut self,
        x: &mut Literal,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let typed = match x {
            Literal::Number(_) => Type::Num,
            Literal::String(_) => Type::Str,
            Literal::List(l) => {
                if l.is_empty() {
                    Type::List(Box::new(Type::Empty))
                } else {
                    let mut ret_types = vec![];
                    for i in 0..l.len() {
                        l[i] = self.check_expr(&mut l[i], None)?;
                        ret_types.push(self.get_expr_type(&l[i]));
                    }
                    let mut prev = None;
                    for t in ret_types {
                        if prev == None {
                            prev = t;
                        } else if prev == t {
                            prev = t;
                        }
                    }
                    let ret = if let Some(t) = prev {
                        match t {
                            Typed::Assigned(ty) => Type::List(Box::new(ty)),
                            Typed::Inferred(ty) => Type::List(Box::new(ty)),
                        }
                    } else {
                        return Err(TypeError::InvalidType {
                            help: "hepl".into(),
                            span: (1, 2).into(),
                        });
                    };
                    ret
                }
            }
            Literal::True => Type::Bool,
            Literal::False => Type::Bool,
            Literal::None => Type::None,
            Literal::Empty => Type::Empty,
            Literal::Eol => Type::None,
        };

        let res = x.clone();
        Ok(Expr::Literal(res, Some(Typed::Assigned(typed))))
    }

    fn visit_unary(
        &mut self,
        x: &mut Unary,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        x.typed = match &x.operator.token {
            TT::Operator(o) => {
                if o == &Op::Not {
                    Some(Typed::Inferred(Type::Bool))
                } else {
                    Some(Typed::Inferred(Type::None))
                }
            }
            _ => unreachable!(),
        };

        let right = self.check_expr(&mut x.right, None)?;
        let right_type = self.get_expr_type(&right).clone().unwrap();

        if self.match_type(&right_type, &x.typed.clone().unwrap()) {
            let res = x.clone();
            Ok(Expr::Unary(res))
        } else {
            Err(TypeError::TypeMismatch {
                help: format!(
                    "expected {:?} found {:?}",
                    x.typed.clone().unwrap(),
                    right_type
                ),
                span: x.operator.span,
            })
        }
    }

    fn visit_binary(
        &mut self,
        x: &mut Binary,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        x.typed = Some(Typed::Inferred(Type::Num));

        let left = self.check_expr(&mut x.left, None)?;
        let left_type = self.get_expr_type(&left);

        let right = self.check_expr(&mut x.right, None)?;
        let right_type = self.get_expr_type(&right);

        if !self.match_type(&left_type.clone().unwrap(), &right_type.clone().unwrap())
            || !(x.typed == left_type)
            || !(x.typed == right_type)
        {
            Err(TypeError::TypeMismatch {
                help: "types dont match".into(),
                span: x.operator.span,
            })
        } else {
            let res = x.clone();
            Ok(Expr::Binary(res))
        }
    }

    fn visit_identifier(
        &mut self,
        x: &mut Identifier,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let ty = x.name.token.to_string();

        if ty == "int" {
            x.typed = Some(Typed::Assigned(Type::Num));
        } else if ty == "str" {
            x.typed = Some(Typed::Assigned(Type::Str));
        } else if ty == "bool" {
            x.typed = Some(Typed::Assigned(Type::Bool));
        } else if ty == "None" {
            x.typed = Some(Typed::Assigned(Type::None));
        } else {
            x.typed = Some(Typed::Inferred(Type::Id(x.name.token.to_string())));
        }

        let res = x.clone();
        Ok(Expr::Identifier(res))
    }

    fn visit_variable(
        &mut self,
        x: &mut Variable,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let var_name = match &x.name.token {
            TT::Identifier(i) => i,
            _ => unreachable!(),
        };
        for (sym, ty) in self.symbols.clone().iter() {
            if sym == var_name {
                x.typed = Some(ty.clone());
                break;
            }
        }
        if x.typed == None {
            x.typed = Some(Typed::Inferred(Type::None));
        }
        let res = x.clone();
        Ok(Expr::Variable(res))
    }

    fn visit_assign(
        &mut self,
        x: &mut Assign,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        x.value = Box::new(self.check_expr(&mut x.value, None)?);
        x.typed = self.get_expr_type(&x.value);

        let res = x.clone();
        Ok(Expr::Assign(res))
    }

    fn visit_logical(
        &mut self,
        x: &mut Logical,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        x.typed = Some(Typed::Assigned(Type::Bool));

        let left = self.check_expr(&mut x.left, None)?;
        let left_type = self.get_expr_type(&left).unwrap();

        let right = self.check_expr(&mut x.right, None)?;
        let right_type = self.get_expr_type(&right).unwrap();

        if self.match_type(&left_type, &right_type) {
            x.right = Box::new(right);
            x.left = Box::new(left);
            x.typed = Some(Typed::Inferred(Type::Bool));
            let res = x.clone();
            Ok(Expr::Logical(res))
        } else {
            Err(TypeError::TypeMismatch {
                help: format!(
                    "left type {:?} and right type {:?} do not match",
                    left_type, right_type
                ),
                span: x.operator.span,
            })
        }
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
        let callee_expr = self.check_expr(&mut x.callee, None)?;
        let callee_type = self.get_expr_type(&callee_expr).unwrap();
        let callee_name = match callee_expr {
            Expr::Identifier(i) => i.name,
            Expr::Variable(v) => v.name,
            _ => unreachable!(),
        };

        let infr_type = if callee_name.token.to_string() == "print" {
            Typed::Inferred(Type::None)
        } else if self.symbols.contains_key(&callee_name.token.to_string()) {
            self.symbols
                .get(&callee_name.token.to_string())
                .unwrap()
                .clone()
        } else {
            Typed::Inferred(Type::None)
        };

        let mut checked_args = vec![];
        for a in &mut x.args {
            checked_args.push(self.check_expr(a, None)?);
        }

        x.args = checked_args;

        if self.match_type(&callee_type, &infr_type) {
            x.typed = Some(callee_type);
            let res = x.clone();
            Ok(Expr::Call(res))
        } else {
            Err(TypeError::TypeMismatch {
                help: format!(
                    "assigned type was {:#?} while initialized type was {:#?}",
                    callee_type, infr_type
                ),
                span: callee_name.span,
            })
        }
    }

    fn visit_index(
        &mut self,
        x: &mut Index,
        state: Option<Vec<String>>,
    ) -> Result<Expr, TypeError> {
        let expr = self.check_expr(&mut x.value, None)?;
        let expr_type = self.get_expr_type(&expr).unwrap();
        let infr_type = Typed::Inferred(Type::Num);

        if self.match_type(&infr_type, &expr_type) {
            x.typed = Some(expr_type);
            let res = x.clone();
            Ok(Expr::Index(res))
        } else {
            Err(TypeError::InvalidType {
                help: format!("types {:?} and {:?} do not match", expr_type, x.typed),
                span: (1, 1).into(),
            })
        }
    }
}
