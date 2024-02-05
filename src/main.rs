use std::env::args;
use std::fs::read;

use miette::{Diagnostic, Result};
use thiserror::Error;

use lexer::{LexError, Lexer};
use parser::{ParseError, Parser};

mod lexer;
mod parser;
mod syntax;
mod token;

#[derive(Error, Debug, Diagnostic)]
#[error("multiple errors encountered")]
#[diagnostic()]
pub struct LexErrors {
    #[source_code]
    source_code: String,
    #[related]
    related: Vec<LexError>,
}

#[derive(Error, Debug, Diagnostic)]
#[error("multiple errors encountered")]
#[diagnostic()]
pub struct ParseErrors {
    #[source_code]
    source_code: String,
    #[related]
    related: Vec<ParseError>,
}

fn main() -> Result<()> {
    let args: Vec<String> = args().collect();
    let source_file = &args[1];
    let code = read(source_file).unwrap();

    let mut lexer = Lexer::new(&code);

    let tokens = lexer.lex_code().map_err(|err_list| LexErrors {
        source_code: String::from_utf8(code.to_owned()).unwrap(),
        related: err_list,
    })?;
    /*
        for t in tokens.clone() {
            println!("{}", t);
        }
    */
    let mut parser = Parser::new(tokens);

    let result = parser.parse().map_err(|err_list| ParseErrors {
        source_code: String::from_utf8(code.to_owned()).unwrap(),
        related: err_list,
    })?;

    for r in result {
        println!("{}", r);
    }

    Ok(())
}
