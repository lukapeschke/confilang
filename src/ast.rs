use crate::token::Token;

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Identifier(expressions::Identifier),
}

#[derive(Debug)]
pub enum Statement {
    Let(statements::Let),
}

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }

    pub fn push(&mut self, s: Statement) {
        self.statements.push(s)
    }

    pub fn repr(&self) -> String {
        format!("{:?}", self.statements)
    }
}

mod expressions {
    #[derive(Debug)]
    pub struct Identifier {
        name: String,
    }

    impl Identifier {
        pub fn new(name: String) -> Identifier {
            Identifier { name: name }
        }
    }
}

pub mod statements {
    use super::expressions::Identifier;
    use super::*;
    use crate::parser;
    pub trait StatementType {
        fn parse(l: &mut parser::Parser) -> Result<Statement, String>;
    }

    #[derive(Debug)]
    pub struct Let {
        ident: expressions::Identifier,
        value: Expression,
    }

    impl StatementType for Let {
        fn parse(l: &mut parser::Parser) -> Result<Statement, String> {
            let ident = match l.peek_token() {
                Some(Token::Ident(id)) => {
                    l.next_token();
                    Ok(id)
                }
                Some(tok) => Err(format!("Expected Ident token, got {:?}", tok)),
                None => Err("Expected Ident token, got None".to_string()),
            }?;

            // Skipping assign;
            if match l.peek_token() {
                Some(Token::Assign) => {
                    l.next_token();
                    false
                }
                _ => true,
            } {
                return Err(format!("Expected Assign token",));
            }

            // FIXME: we only suport "let" x = y;
            let value = match l.peek_token() {
                Some(Token::Ident(val)) => {
                    l.next_token();
                    Ok(val)
                }
                Some(tok) => Err(format!("Expected Ident token, got {:?}", tok)),
                None => Err("Expected Ident token, got None".to_string()),
            }?;

            // Skipping semicolon
            if match l.peek_token() {
                Some(Token::Semicolon) => {
                    l.next_token();
                    false
                }
                _ => true,
            } {
                return Err(format!("Expected Semicolon token",));
            }

            Ok(Statement::Let(Let {
                ident: Identifier::new("ident".to_string()),
                value: Expression::Identifier(Identifier::new("value".to_string())),
            }))
        }
    }
}
