use std::env::args;
use std::fs::{read, File};

use lexer::Lexer;

mod lexer;
mod token;

fn main() {
    let args: Vec<String> = args().collect();
    let source_file = &args[1];
    let code = read(source_file).unwrap();

    let mut lexer = Lexer::new(&code);

    let result = lexer.lex_code();

    for r in result.unwrap() {
        println!("{}", r);
    }
}
