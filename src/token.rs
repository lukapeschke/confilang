use crate::ast;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Basic
    Illegal(String),
    Eof,

    // Identifiers and literals
    Ident(String),
    Int(i32),
    Float(f32),

    // Operators
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Lt,
    Gt,

    // Delimiters
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Keywords
    Let,
    Fn,
    True,
    False,
    If,
    Else,
    Return,
    And,
    Or,

    // Multi char operators
    Equals,
    Differs,
    Ge,
    Le,
}

impl Token {
    pub fn parse_prefix(&self) -> Result<ast::Expression, String> {
        match self {
            // ident is a &String
            Token::Ident(ident) => Ok(ast::Expression::Identifier(
                ast::expressions::Identifier::new(ident.clone()),
            )),
            _ => Err(format!("Unsupported prefix token {:?}", self)),
        }
    }
    pub fn parse_infix(&self) -> Result<ast::Expression, String> {
        match self {
            _ => Err(format!("Unsupported infix token {:?}", self)),
        }
    }
}
