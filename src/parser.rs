use crate::ast::statements::StatementType;
use crate::ast::*;
use crate::lexer;
use crate::token::Token;

pub struct Parser<'a> {
    lex: &'a mut lexer::Lexer<'a>,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(l: &'a mut lexer::Lexer<'a>) -> Option<Parser<'a>> {
        let peek = l.next_token()?;
        let p = Parser {
            lex: l,
            cur_token: None,
            peek_token: Some(peek),
        };
        Some(p)
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lex.next_token();
        self.cur_token()
    }

    pub fn peek_token(&self) -> Option<Token> {
        self.peek_token.clone()
    }

    pub fn cur_token(&self) -> Option<Token> {
        self.cur_token.clone()
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut p = Program::new();
        while let Some(tok) = self.next_token() {
            let statement = match tok {
                Token::Let => statements::Let::parse(self),
                Token::Return => statements::Return::parse(self),
                _ => Err(format!("Unmatched token {:?}", tok)),
            }?;
            p.push(statement);
        }
        Ok(p)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn test_parse_x(input: String, expected: &Vec<Statement>) {
        let mut lex = Lexer::new(&input).unwrap();
        let mut parser = Parser::new(&mut lex).unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.statements(), expected)
    }

    #[test]
    fn test_parse_let() {
        test_parse_x(
            "let a = b;".to_string(),
            &vec![Statement::Let(statements::Let::new(
                expressions::Identifier::new("a".to_string()),
                Expression::Identifier(expressions::Identifier::new("b".to_string())),
            ))],
        );
    }

    #[test]
    fn test_parse_return() {
        test_parse_x(
            "return r;".to_string(),
            &vec![Statement::Return(statements::Return::new(
                expressions::Identifier::new("r".to_string()),
            ))],
        );
    }
}
