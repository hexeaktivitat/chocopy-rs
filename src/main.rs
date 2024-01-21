use std::env::args;
use std::fs::{read, File};

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use lexer::{LexError, Lexer};

mod lexer;
mod parser;
mod syntax;
mod token;

#[derive(Error, Debug, Diagnostic)]
#[error("multiple errors encountered")]
#[diagnostic()]
pub struct MultiError {
    #[source_code]
    source_code: String,
    #[related]
    related: Vec<LexError>,
}

fn main() -> Result<()> {
    let args: Vec<String> = args().collect();
    let source_file = &args[1];
    let code = read(source_file).unwrap();

    let mut lexer = Lexer::new(&code);

    let result = lexer.lex_code().map_err(|err_list| MultiError {
        source_code: String::from_utf8(code.to_owned()).unwrap(),
        related: err_list,
    })?;

    for r in result {
        println!("{}", r);
    }

    Ok(())
}
