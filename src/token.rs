use crate::ast;
use crate::parser;

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
    fn parse_prefix_expression(&self, p: &mut parser::Parser) -> Result<ast::Expression, String> {
        // If we're here, we assume that self if one of [Bang, Minus]
        p.next_token();
        let expr = p.parse_expression(&parser::Precedence::Prefix)?;

        Ok(ast::Expression::Prefix(ast::expressions::Prefix::new(
            self.clone(),
            expr,
        )))
    }

    fn parse_grouped_expression(&self, p: &mut parser::Parser) -> Result<ast::Expression, String> {
        // We expect to have a token after the (
        if let Some(_) = p.next_token() {
        } else {
            return Err("Expected a token after LeftParen".to_string());
        };

        let exp = p.parse_expression_lowest()?;

        // Token after expression should be )
        match p.peek_token() {
            Some(Token::RightParen) => {
                p.next_token();
                Ok(exp)
            }
            Some(tok) => Err(format!(
                "Expected a RightParen token after grouped expression, got {:?}",
                tok
            )),
            None => {
                Err("Expected a RightParen token after grouped expression, got None".to_string())
            }
        }
    }

    fn parse_if(&self, p: &mut parser::Parser) -> Result<ast::Expression, String> {
        // // means we expect to skip a parenthesis
        let has_paren = if let Some(Token::LeftParen) = p.peek_token() {
            p.next_token();
            true
        } else {
            false
        };
        p.next_token();
        let condition = p.parse_expression_lowest()?;

        if let Some(Token::LeftBrace) = p.peek_token() {
            p.next_token();
        } else {
            return Err(format!(
                "Expected LeftBrace token, got {:?}",
                p.peek_token()
            ));
        }

        let consequence = p.parse_block_statement()?;

        let alternative = match p.peek_token() {
            Some(Token::Else) => {
                p.next_token();
                if let Some(Token::LeftBrace) = p.peek_token() {
                    p.next_token();
                } else {
                    return Err(format!(
                        "Expected LeftBrace token, got {:?}",
                        p.peek_token()
                    ));
                }
                Some(p.parse_block_statement()?)
            }
            _ => None,
        };

        Ok(ast::Expression::If(ast::expressions::If::new(
            condition,
            consequence,
            alternative,
        )))
    }

    fn parse_fn_params(
        &self,
        p: &mut parser::Parser,
    ) -> Result<Vec<ast::expressions::Identifier>, String> {
        let mut output = Vec::new();
        // let mut output = Vec::new::<ast::expressions::Identifier>();

        while let Some(tok) = p.peek_token() {
            match tok {
                // This is required for empty params: fn a() {}
                Token::RightParen => break,
                Token::Ident(id) => {
                    output.push(ast::expressions::Identifier::new(id));
                    p.next_token();

                    match p.peek_token() {
                        // Skipping Comma
                        Some(Token::Comma) => {
                            p.next_token();
                        }
                        // We reached the end of the params
                        Some(Token::RightParen) => {
                            break;
                        }
                        _ => {
                            return Err(format!(
                                "Expected comma or left parenthesis, got {:?}",
                                p.peek_token()
                            ))
                        }
                    }
                }
                _ => {
                    return Err(format!(
                        "Expected parameter or right parenthesis, got {:?}",
                        p.peek_token()
                    ))
                }
            }
        }

        // Making sure we have a closing parenthesis
        if let Some(Token::RightParen) = p.peek_token() {
            p.next_token();
        } else {
            return Err(format!(
                "Expected right parenthesis, got {:?}",
                p.peek_token()
            ));
        }

        Ok(output)
    }

    fn parse_fn_litteral(&self, p: &mut parser::Parser) -> Result<ast::Expression, String> {
        // Skipping first paren
        if let Some(Token::LeftParen) = p.peek_token() {
            p.next_token();
        } else {
            return Err(format!("Expected left paren, got {:?}", p.peek_token()));
        }

        let params = self.parse_fn_params(p)?;

        // Skipping left brace
        if let Some(Token::LeftBrace) = p.peek_token() {
            p.next_token();
        } else {
            return Err(format!("Expected left brace, got {:?}", p.peek_token()));
        }

        let body = p.parse_block_statement()?;
        Ok(ast::Expression::Fn(ast::expressions::Fn::new(
            body, &params,
        )))
    }

    pub fn parse_prefix(&self, p: &mut parser::Parser) -> Result<ast::Expression, String> {
        match self {
            Token::Ident(ident) => Ok(ast::Expression::Identifier(
                // ident is a &String, so we need to clone
                ast::expressions::Identifier::new(ident.clone()),
            )),
            Token::Int(i) => Ok(ast::Expression::Int(ast::expressions::Int::new(*i))),
            Token::Float(f) => Ok(ast::Expression::Float(ast::expressions::Float::new(*f))),
            Token::Bang | Token::Minus => self.parse_prefix_expression(p),
            Token::True => Ok(ast::Expression::Boolean(ast::expressions::Boolean::new(
                true,
            ))),
            Token::False => Ok(ast::Expression::Boolean(ast::expressions::Boolean::new(
                false,
            ))),
            Token::LeftParen => self.parse_grouped_expression(p),
            Token::If => self.parse_if(p),
            Token::Fn => self.parse_fn_litteral(p),
            _ => Err(format!("Unsupported prefix token {:?}", self)),
        }
    }

    pub fn parse_infix(
        &self,
        p: &mut parser::Parser,
        left: ast::Expression,
    ) -> Result<ast::Expression, String> {
        let precedence = p.cur_precedence();
        p.next_token();
        let right = p.parse_expression(&precedence)?;
        Ok(ast::Expression::Infix(ast::expressions::Infix::new(
            self.clone(),
            left,
            right,
        )))
    }

    // Returns a Token's precedence
    pub fn precedence(&self) -> parser::Precedence {
        match self {
            Token::Equals => parser::Precedence::Equals,
            Token::Differs => parser::Precedence::Equals,
            Token::Lt => parser::Precedence::LessGreater,
            Token::Gt => parser::Precedence::LessGreater,
            Token::Plus => parser::Precedence::Sum,
            Token::Minus => parser::Precedence::Sum,
            Token::Slash => parser::Precedence::Product,
            Token::Asterisk => parser::Precedence::Product,
            _ => parser::Precedence::Lowest,
        }
    }

    pub fn repr(&self) -> String {
        match self {
            // Identifiers and literals
            Token::Ident(s) => s.clone(),
            Token::Int(i) => format!("{}", i),
            Token::Float(f) => format!("{}", f),

            // Operators
            Token::Assign => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::Bang => "!".to_string(),
            Token::Lt => "<".to_string(),
            Token::Gt => ">".to_string(),

            // Delimiters
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::LeftParen => "(".to_string(),
            Token::RightParen => ")".to_string(),
            Token::LeftBrace => "[".to_string(),
            Token::RightBrace => "]".to_string(),

            // Keywords
            Token::Let => "let".to_string(),
            Token::Fn => "fn".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Return => "return".to_string(),
            Token::And => "repr".to_string(),
            Token::Or => "or".to_string(),

            // Multi char operators
            Token::Equals => "==".to_string(),
            Token::Differs => "!=".to_string(),
            Token::Ge => ">=".to_string(),
            Token::Le => "<=".to_string(),
            _ => "<Err>".to_string(),
        }
    }
}
