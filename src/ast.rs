use crate::token::Token;

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(expressions::Identifier),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(statements::Let),
    Return(statements::Return),
    ExpressionStatement(statements::ExpressionStatement),
}

#[derive(Debug, PartialEq)]
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

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

pub mod expressions {
    #[derive(Debug, PartialEq)]
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

    #[derive(Debug, PartialEq)]
    pub struct Let {
        ident: expressions::Identifier,
        value: Expression,
    }

    impl Let {
        pub fn new(ident: expressions::Identifier, value: Expression) -> Let {
            Let {
                ident: ident,
                value: value,
            }
        }
    }

    impl StatementType for Let {
        fn parse(p: &mut parser::Parser) -> Result<Statement, String> {
            let ident = match p.peek_token() {
                Some(Token::Ident(id)) => {
                    p.next_token();
                    Ok(id)
                }
                Some(tok) => Err(format!("Expected Ident token, got {:?}", tok)),
                None => Err("Expected Ident token, got None".to_string()),
            }?;

            // Skipping assign;
            if match p.peek_token() {
                Some(Token::Assign) => {
                    p.next_token();
                    false
                }
                _ => true,
            } {
                return Err(format!("Expected Assign token",));
            }

            // FIXME: we only suport "let" x = y;
            let value = match p.peek_token() {
                Some(Token::Ident(val)) => {
                    p.next_token();
                    Ok(val)
                }
                Some(tok) => Err(format!("Expected Ident token, got {:?}", tok)),
                None => Err("Expected Ident token, got None".to_string()),
            }?;

            // Skipping semicolon
            if match p.peek_token() {
                Some(Token::Semicolon) => {
                    p.next_token();
                    false
                }
                _ => true,
            } {
                return Err(format!("Expected Semicolon token",));
            }

            Ok(Statement::Let(Let::new(
                Identifier::new(ident),
                Expression::Identifier(Identifier::new(value)),
            )))
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Return {
        expr: Expression,
    }

    impl Return {
        pub fn new(expr: expressions::Identifier) -> Return {
            Return {
                expr: Expression::Identifier(expr),
            }
        }
    }

    impl StatementType for Return {
        fn parse(p: &mut parser::Parser) -> Result<Statement, String> {
            let ident = match p.peek_token() {
                Some(Token::Ident(id)) => {
                    p.next_token();
                    Ok(id)
                }
                Some(tok) => Err(format!("Expected Ident token, got {:?}", tok)),
                None => Err("Exepected Ident token, got None".to_string()),
            }?;

            match p.peek_token() {
                Some(Token::Semicolon) => {
                    p.next_token();
                    Ok(Statement::Return(Return::new(Identifier::new(ident))))
                }
                Some(tok) => Err(format!("Expected Semicolon token, got {:?}", tok)),
                None => Err("Exepected Ident token, got None".to_string()),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct ExpressionStatement {
        expr: Expression,
    }

    impl ExpressionStatement {
        pub fn new(expr: Expression) -> ExpressionStatement {
            ExpressionStatement { expr: expr }
        }
    }

    impl StatementType for ExpressionStatement {
        fn parse(p: &mut parser::Parser) -> Result<Statement, String> {
            let expression = p.parse_expression_lowest()?;

            // Skipping semicolon
            match p.peek_token() {
                Some(Token::Semicolon) => {
                    p.next_token();
                }
                _ => (),
            }

            Ok(Statement::ExpressionStatement(ExpressionStatement::new(
                expression,
            )))
        }
    }
}
