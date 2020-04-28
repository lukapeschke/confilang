use crate::token::Token;

pub trait Representable {
    fn repr(&self) -> String;
}

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(expressions::Identifier),
    Int(expressions::Int),
    Float(expressions::Float),
    Prefix(expressions::Prefix),
    Infix(expressions::Infix),
    Boolean(expressions::Boolean),
}

impl Representable for Expression {
    fn repr(&self) -> String {
        match self {
            Expression::Identifier(s) => s.repr(),
            Expression::Int(s) => s.repr(),
            Expression::Float(s) => s.repr(),
            Expression::Prefix(s) => s.repr(),
            Expression::Infix(s) => s.repr(),
            Expression::Boolean(s) => s.repr(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(statements::Let),
    Return(statements::Return),
    ExpressionStatement(statements::ExpressionStatement),
}

impl Representable for Statement {
    fn repr(&self) -> String {
        match self {
            Statement::Let(s) => s.repr(),
            Statement::Return(s) => s.repr(),
            Statement::ExpressionStatement(s) => s.repr(),
        }
    }
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

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

impl Representable for Program {
    fn repr(&self) -> String {
        self.statements()
            .iter()
            .fold("".to_string(), |acc, x| match acc.chars().count() {
                0 => x.repr(),
                _ => format!("{}\n{}", acc, x.repr()),
            })
    }
}

pub mod expressions {

    use super::Expression;
    use super::Representable;
    use super::Token;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Identifier {
        name: String,
    }

    impl Identifier {
        pub fn new(name: String) -> Identifier {
            Identifier { name: name }
        }
    }

    impl Representable for Identifier {
        fn repr(&self) -> String {
            self.name.clone()
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Int {
        val: i32,
    }

    impl Int {
        pub fn new(i: i32) -> Int {
            Int { val: i }
        }
    }

    impl Representable for Int {
        fn repr(&self) -> String {
            self.val.to_string()
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Float {
        val: f32,
    }

    impl Float {
        pub fn new(f: f32) -> Float {
            Float { val: f }
        }
    }

    impl Representable for Float {
        fn repr(&self) -> String {
            self.val.to_string()
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Prefix {
        tok: Token,
        expr: Box<Expression>,
    }

    impl Prefix {
        pub fn new(tok: Token, expr: Expression) -> Prefix {
            Prefix {
                tok: tok,
                expr: Box::new(expr),
            }
        }
    }

    impl Representable for Prefix {
        fn repr(&self) -> String {
            format!("({}{})", self.tok.repr(), self.expr.repr())
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Infix {
        tok: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    }

    impl Infix {
        pub fn new(tok: Token, left: Expression, right: Expression) -> Infix {
            Infix {
                tok: tok,
                left: Box::new(left),
                right: Box::new(right),
            }
        }
    }

    impl Representable for Infix {
        fn repr(&self) -> String {
            format!(
                "({} {} {})",
                self.left.repr(),
                self.tok.repr(),
                self.right.repr()
            )
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Boolean {
        val: bool,
    }

    impl Boolean {
        pub fn new(val: bool) -> Boolean {
            Boolean { val: val }
        }
    }

    impl Representable for Boolean {
        fn repr(&self) -> String {
            self.val.to_string()
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

    impl Representable for Let {
        fn repr(&self) -> String {
            format!("let {} = {};", self.ident.repr(), self.value.repr())
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

    impl Representable for Return {
        fn repr(&self) -> String {
            format!("return {};", self.expr.repr())
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

    impl Representable for ExpressionStatement {
        fn repr(&self) -> String {
            format!("{};", self.expr.repr())
        }
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

#[cfg(test)]
pub mod tests {
    use super::Representable;
    use crate::lexer;
    use crate::parser;

    pub fn test_repr(input: &str, expected: &str) {
        let input_str = input.to_string();
        let mut lex = lexer::Lexer::new(&input_str).unwrap();
        let mut parser = parser::Parser::new(&mut lex).unwrap();
        assert_eq!(expected.to_string(), parser.parse_program().unwrap().repr())
    }

    #[test]
    fn test_repr_let() {
        test_repr("let a=b;", "let a = b;");
    }

    #[test]
    fn test_repr_return() {
        test_repr("return toto;", "return toto;");
    }

    #[test]
    fn test_repr_expression() {
        test_repr("toto;", "toto;");
    }
}
