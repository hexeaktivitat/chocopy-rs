use miette::Error;

use crate::token::{Token, TokenType};

struct Parser {
    source: String,
    tokens: Vec<Token>,
}

impl Parser {
    fn new(source: &str) -> Self {
        Self {
            source: source.to_string(),
            tokens: Vec::new(),
        }
    }
}
