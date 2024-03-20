use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::syntax::{ExprVisitor, StmtVisitor};

#[derive(Error, Diagnostic, Debug)]
pub enum AsmError {
    #[error("Assembly error")]
    AssemblyError {
        #[help]
        help: String,
        #[label]
        span: SourceSpan,
    },
}

#[derive(Debug, Clone)]
struct Asm {
    opcodes: Vec<OpCode>,
}

#[derive(Debug, Clone, Copy)]
enum OpCode {
    // Arithmetic Ops
    AddI(Register, Register, i32), // Add Immediate

    // Load / Store Ops
    Sw(Register, i32),
}

#[derive(Debug, Clone, Copy)]
enum Register {
    Sp, // Stack pointer (special register)
}

struct Assembler {}

impl Assembler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn assemble() {}
}

impl StmtVisitor<Result<Asm, AsmError>, Option<Vec<String>>> for Assembler {
    fn visit_var(
        &mut self,
        x: &mut crate::syntax::Var,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_func(
        &mut self,
        x: &mut crate::syntax::Func,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_expression(
        &mut self,
        x: &mut crate::syntax::Expression,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_block(
        &mut self,
        x: &mut crate::syntax::Block,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_if(
        &mut self,
        x: &mut crate::syntax::If,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_return(
        &mut self,
        x: &mut crate::syntax::Return,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_print(
        &mut self,
        x: &mut crate::syntax::Print,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }
}

impl ExprVisitor<Result<Asm, AsmError>, Option<Vec<String>>> for Assembler {
    fn visit_literal(
        &mut self,
        x: &mut crate::syntax::Literal,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_unary(
        &mut self,
        x: &mut crate::syntax::Unary,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_binary(
        &mut self,
        x: &mut crate::syntax::Binary,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_identifier(
        &mut self,
        x: &mut crate::syntax::Identifier,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_variable(
        &mut self,
        x: &mut crate::syntax::Variable,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_assign(
        &mut self,
        x: &mut crate::syntax::Assign,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        let mut res = Asm { opcodes: vec![] };

        // Increment stack pointer by 32 bits for assignment op
        res.opcodes
            .push(OpCode::AddI(Register::Sp, Register::Sp, -32));

        Ok(res)
    }

    fn visit_logical(
        &mut self,
        x: &mut crate::syntax::Logical,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_grouping(
        &mut self,
        x: &mut crate::syntax::Grouping,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_call(
        &mut self,
        x: &mut crate::syntax::Call,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }

    fn visit_index(
        &mut self,
        x: &mut crate::syntax::Index,
        state: Option<Vec<String>>,
    ) -> Result<Asm, AsmError> {
        todo!()
    }
}
