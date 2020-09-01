use crate::ast::statements::StatementType;
use crate::ast::*;
use crate::lexer;
use crate::token::Token;

#[derive(Clone, Copy)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix, // -x or !x
    Call,
    Index,
}

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

    pub fn peek_precedence(&self) -> Precedence {
        if let Some(token) = self.peek_token() {
            token.precedence()
        } else {
            Precedence::Lowest
        }
    }

    pub fn cur_precedence(&self) -> Precedence {
        if let Some(token) = self.cur_token() {
            token.precedence()
        } else {
            Precedence::Lowest
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut p = Program::new();
        while let Some(tok) = self.next_token() {
            let statement = self.parse_statement(&tok)?;
            p.push(statement);
        }
        Ok(p)
    }

    pub fn parse_statement(&mut self, token: &Token) -> Result<Statement, String> {
        match token {
            Token::Let => statements::Let::parse(self),
            Token::Return => statements::Return::parse(self),
            _ => statements::ExpressionStatement::parse(self),
            // _ => Err(format!("Unmatched token {:?}", token)),
        }
    }

    pub fn parse_block_statement(&mut self) -> Result<statements::Block, String> {
        if let Statement::Block(block) = statements::Block::parse(self)? {
            Ok(block)
        } else {
            Err("Couldn't parse a Block statement".to_string())
        }
    }

    pub fn parse_expression_lowest(&mut self) -> Result<Expression, String> {
        self.parse_expression(&Precedence::Lowest)
    }

    fn parse_expression_should_break(&self, prec: &Precedence) -> bool {
        let token_is_semicolumn_or_none = match self.peek_token() {
            Some(Token::Semicolon) | None => true,
            _ => false,
        };
        let precedence_is_higher_as_next = *prec as i32 >= self.peek_precedence() as i32;
        token_is_semicolumn_or_none || precedence_is_higher_as_next
    }

    pub fn parse_expression(&mut self, prec: &Precedence) -> Result<Expression, String> {
        if let Some(token) = self.cur_token() {
            let mut left = token.parse_prefix(self)?;

            // looping until we reach a semicolumn or our precedence is
            // superior to the next token's precedence
            while !self.parse_expression_should_break(prec) {
                if let Some(token) = self.next_token() {
                    // We have an Option<Result<Expression>>. None means that no
                    // infix parsing functon is associated to the left token
                    if let Some(expr_res) = token.parse_infix(self, left.clone()) {
                        left = expr_res?;
                    } else {
                        return Ok(left);
                    }
                }
            }

            Ok(left)
        } else {
            Err("No token to parse".to_string())
        }
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
    fn test_parse_let_repr() {
        let v = [
            ("let a = 4+5", "let a = (4 + 5);"),
            ("let tutu = toto", "let tutu = toto;"),
        ];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_parse_return() {
        test_parse_x(
            "return r;".to_string(),
            &vec![Statement::Return(statements::Return::new(
                Expression::Identifier(expressions::Identifier::new("r".to_string())),
            ))],
        );
    }

    #[test]
    fn test_parse_return_repr() {
        let v = [
            ("return 4+5", "return (4 + 5);"),
            ("return toto", "return toto;"),
        ];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_parse_expression_statement_ident() {
        test_parse_x(
            "super_ident;".to_string(),
            &vec![Statement::ExpressionStatement(
                statements::ExpressionStatement::new(Expression::Identifier(
                    expressions::Identifier::new("super_ident".to_string()),
                )),
            )],
        );
    }

    #[test]
    fn test_parse_expression_statement_int_float() {
        test_parse_x(
            "42;13.37".to_string(),
            &vec![
                Statement::ExpressionStatement(statements::ExpressionStatement::new(
                    Expression::Int(expressions::Int::new(42)),
                )),
                Statement::ExpressionStatement(statements::ExpressionStatement::new(
                    Expression::Float(expressions::Float::new(13.37)),
                )),
            ],
        );
    }

    #[test]
    fn test_parse_expression_statement_bang_prefix() {
        test_parse_x(
            "!coucou".to_string(),
            &vec![Statement::ExpressionStatement(
                statements::ExpressionStatement::new(Expression::Prefix(expressions::Prefix::new(
                    Token::Bang,
                    Expression::Identifier(expressions::Identifier::new("coucou".to_string())),
                ))),
            )],
        );
    }

    #[test]
    fn test_parse_expression_statement_minus_prefix() {
        test_parse_x(
            "-coucou;".to_string(),
            &vec![Statement::ExpressionStatement(
                statements::ExpressionStatement::new(Expression::Prefix(expressions::Prefix::new(
                    Token::Minus,
                    Expression::Identifier(expressions::Identifier::new("coucou".to_string())),
                ))),
            )],
        );
    }

    #[test]
    fn test_parse_infix() {
        let cases = vec![
            ("42 + 42;", Token::Plus, 42, 42),
            ("42+42;", Token::Plus, 42, 42),
            ("42 -84;", Token::Minus, 42, 84),
            ("42* 84", Token::Asterisk, 42, 84),
            ("42 < 84;", Token::Lt, 42, 84),
            ("42 > 84;", Token::Gt, 42, 84),
            ("42 == 84;", Token::Equals, 42, 84),
            ("42 != 84;", Token::Differs, 42, 84),
        ];
        for case in cases {
            let input = case.0.to_string();
            let expected_infix = expressions::Infix::new(
                case.1,
                Expression::Int(expressions::Int::new(case.2)),
                Expression::Int(expressions::Int::new(case.3)),
            );
            let expected_output = vec![Statement::ExpressionStatement(
                statements::ExpressionStatement::new(Expression::Infix(expected_infix)),
            )];
            test_parse_x(input, &expected_output);
        }
    }

    #[test]
    fn test_parse_boolean() {
        let cases = vec![
            ("true == true;", Token::Equals, true, true),
            ("true != false;", Token::Differs, true, false),
            ("false != false;", Token::Differs, false, false),
            ("false == true;", Token::Equals, false, true),
        ];
        for case in cases {
            let input = case.0.to_string();
            let expected_infix = expressions::Infix::new(
                case.1,
                Expression::Boolean(expressions::Boolean::new(case.2)),
                Expression::Boolean(expressions::Boolean::new(case.3)),
            );
            let expected_output = vec![Statement::ExpressionStatement(
                statements::ExpressionStatement::new(Expression::Infix(expected_infix)),
            )];
            test_parse_x(input, &expected_output);
        }
    }

    use crate::ast::tests::test_repr;

    #[test]
    fn test_parse_grouped() {
        let v = [
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("-5*3", "((-5) * 3)"),
        ];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_if() {
        let v = [
            (
                "if i { true;};",
                "if (i) {
true
}",
            ),
            (
                "if false {2+5} else {toto}",
                "if (false) {
(2 + 5)
} else {
toto
}",
            ),
        ];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_fn() {
        let v = [
            (
                "fn(x, y) { x + y; }",
                "fn (x, y) {
(x + y)
}",
            ),
            ("fn () {return x;}", "fn () {\nreturn x;\n}"),
        ];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_call() {
        let v = [("add(1,2*3,4+5)", "add(1, (2 * 3), (4 + 5))")];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_str() {
        let v = [
            ("\"hello\"", "\"hello\""),
            ("\"hello world with spaces\"", "\"hello world with spaces\""),
        ];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_array() {
        let v = [("[1, 2*3, 8]", "[1, (2 * 3), 8]")];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_array_index() {
        let v = [
            ("my_array[1]", "(my_array[1])"),
            ("my_array[1+2]", "(my_array[(1 + 2)])"),
        ];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    #[test]
    fn test_array_index_precedence() {
        let v = [(
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        )];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }

    // FIXME(lpeschke): Flaky, keys may be swapped
    #[test]
    fn test_hashmap() {
        let v = [
            ("{1: 2, \"a\": \"b\"}", "{1: 2, \"a\": \"b\"}"),
            ("{}", "{}"),
        ];
        for t in v.iter() {
            test_repr(t.0, t.1);
        }
    }
}
