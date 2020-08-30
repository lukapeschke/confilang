use crate::ast;
use crate::parser;
use std::mem;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Basic
    Illegal(String),
    Eof,

    // Identifiers and literals
    Ident(String),
    Int(i32),
    Float(f32),
    Str(String),

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
    Colon,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

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

    fn ensure_right_paren(&self, p: &mut parser::Parser) -> Result<(), String> {
        if let Some(Token::RightParen) = p.peek_token() {
            p.next_token();
            Ok(())
        } else {
            Err(format!(
                "Expected right parenthesis, got {:?}",
                p.peek_token()
            ))
        }
    }

    fn parse_grouped_expression(&self, p: &mut parser::Parser) -> Result<ast::Expression, String> {
        // We expect to have a token after the (
        if p.next_token().is_none() {
            return Err("Expected a token after LeftParen".to_string());
        };

        let exp = p.parse_expression_lowest()?;

        // Token after expression should be )
        self.ensure_right_paren(p)?;
        Ok(exp)
    }

    fn parse_if(&self, p: &mut parser::Parser) -> Result<ast::Expression, String> {
        // means we expect to skip a parenthesis
        let has_paren = if let Some(Token::LeftParen) = p.peek_token() {
            p.next_token();
            true
        } else {
            false
        };
        p.next_token();
        let condition = p.parse_expression_lowest()?;

        // See https://github.com/rust-lang/rust/issues/53667
        // if has_paren && let Some(Token::RightParen) = p.peek_token() {
        if has_paren {
            if let Some(Token::RightParen) = p.peek_token() {
                p.next_token();
            }
        }

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

        self.ensure_right_paren(p)?;

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

    fn parse_expression_list(
        &self,
        p: &mut parser::Parser,
        stop_type: Token,
    ) -> Result<Vec<ast::Expression>, String> {
        let mut output = Vec::new();
        let peek_token_is_discriminant = |tok: Option<Token>| match tok {
            Some(tok) if mem::discriminant(&tok) == mem::discriminant(&stop_type) => true,
            _ => false,
        };

        // Empty list
        if peek_token_is_discriminant(p.peek_token()) {
            p.next_token();
            return Ok(output);
        }

        // Moving on token after opening token ( "(" or "[")
        p.next_token();
        output.push(p.parse_expression_lowest()?);

        while let Some(Token::Comma) = p.peek_token() {
            // Moving on comma
            p.next_token();
            // Moving on token after comma
            p.next_token();
            output.push(p.parse_expression_lowest()?);
        }

        // Ensuring that list is indeed closed by stop_type
        if peek_token_is_discriminant(p.peek_token()) {
            p.next_token();
        } else {
            return Err(format!(
                "Expected right parenthesis, got {:?}",
                p.peek_token()
            ));
        }

        Ok(output)
    }

    fn parse_array(&self, p: &mut parser::Parser) -> Result<ast::Expression, String> {
        Ok(ast::Expression::Array(ast::expressions::Array::new(
            &self.parse_expression_list(p, Token::RightBracket)?,
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
            Token::Str(s) => Ok(ast::Expression::Str(ast::expressions::Str::new(s))),
            Token::Bang | Token::Minus => self.parse_prefix_expression(p),
            Token::True => Ok(ast::Expression::Boolean(ast::expressions::Boolean::new(
                true,
            ))),
            Token::False => Ok(ast::Expression::Boolean(ast::expressions::Boolean::new(
                false,
            ))),
            Token::LeftParen => self.parse_grouped_expression(p),
            Token::LeftBracket => self.parse_array(p),
            Token::If => self.parse_if(p),
            Token::Fn => self.parse_fn_litteral(p),
            _ => Err(format!("Unsupported prefix token {:?}", self)),
        }
    }

    fn parse_infix_default(
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

    fn parse_call(
        &self,
        p: &mut parser::Parser,
        // can be an identifier or a function literal
        callable: ast::Expression,
    ) -> Result<ast::Expression, String> {
        let params = self.parse_expression_list(p, Token::RightParen)?;
        Ok(ast::Expression::Call(ast::expressions::Call::new(
            callable, &params,
        )))
    }

    fn parse_index(
        &self,
        p: &mut parser::Parser,
        left: &ast::Expression,
    ) -> Result<ast::Expression, String> {
        p.next_token();
        let index = p.parse_expression_lowest()?;

        if let Some(Token::RightBracket) = p.peek_token() {
            p.next_token();
            Ok(ast::Expression::Index(ast::expressions::Index::new(
                left, &index,
            )))
        } else {
            Err(format!(
                "Expected a closing bracket got {:?}",
                p.peek_token()
            ))
        }
    }

    pub fn parse_infix(
        &self,
        p: &mut parser::Parser,
        left: ast::Expression,
    ) -> Option<Result<ast::Expression, String>> {
        match self {
            Token::LeftParen => Some(self.parse_call(p, left)),
            Token::LeftBracket => Some(self.parse_index(p, &left)),
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Equals
            | Token::Differs
            | Token::Lt
            | Token::Le
            | Token::Ge
            | Token::Gt => Some(self.parse_infix_default(p, left)),
            // No parsing function associated
            _ => None,
        }
    }

    // Returns a Token's precedence
    pub fn precedence(&self) -> parser::Precedence {
        match self {
            Token::Equals => parser::Precedence::Equals,
            Token::Differs => parser::Precedence::Equals,
            Token::Lt => parser::Precedence::LessGreater,
            Token::Gt => parser::Precedence::LessGreater,
            Token::Le => parser::Precedence::LessGreater,
            Token::Ge => parser::Precedence::LessGreater,
            Token::Plus => parser::Precedence::Sum,
            Token::Minus => parser::Precedence::Sum,
            Token::Slash => parser::Precedence::Product,
            Token::Asterisk => parser::Precedence::Product,
            Token::LeftParen => parser::Precedence::Call,
            Token::LeftBracket => parser::Precedence::Index,
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
            Token::And => "and".to_string(),
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
