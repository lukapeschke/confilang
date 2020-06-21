use crate::token::Token;

pub trait Representable {
    fn repr(&self) -> String;
}

pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Identifier(expressions::Identifier),
    Int(expressions::Int),
    Float(expressions::Float),
    Prefix(expressions::Prefix),
    Infix(expressions::Infix),
    Boolean(expressions::Boolean),
    If(expressions::If),
    Fn(expressions::Fn),
    Call(expressions::Call),
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
            Expression::If(s) => s.repr(),
            Expression::Fn(s) => s.repr(),
            Expression::Call(s) => s.repr(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let(statements::Let),
    Return(statements::Return),
    ExpressionStatement(statements::ExpressionStatement),
    Block(statements::Block),
}

impl Representable for Statement {
    fn repr(&self) -> String {
        match self {
            Statement::Let(s) => s.repr(),
            Statement::Return(s) => s.repr(),
            Statement::ExpressionStatement(s) => s.repr(),
            Statement::Block(s) => s.repr(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

    pub fn as_node(&self) -> Node {
        Node::Program(self.clone())
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

    use super::*;

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

        pub fn value(&self) -> i32 {
            self.val
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

    #[derive(Clone, Debug, PartialEq)]
    pub struct If {
        condition: Box<Expression>,
        consequence: statements::Block,
        alternative: Option<statements::Block>,
    }

    impl If {
        pub fn new(
            cond: Expression,
            cons: statements::Block,
            alt: Option<statements::Block>,
        ) -> If {
            If {
                condition: Box::new(cond),
                consequence: cons,
                alternative: alt,
            }
        }
    }

    impl Representable for If {
        fn repr(&self) -> String {
            let output = format!("if ({}) {}", self.condition.repr(), self.consequence.repr());
            if let Some(alt) = &self.alternative {
                format!("{} else {}", output, alt.repr())
            } else {
                output
            }
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Fn {
        body: statements::Block,
        params: Vec<Identifier>,
    }

    impl Fn {
        pub fn new(body: statements::Block, params: &Vec<Identifier>) -> Fn {
            Fn {
                body: body,
                params: params.to_vec(),
            }
        }
    }

    impl Representable for Fn {
        fn repr(&self) -> String {
            let params = self
                .params
                .iter()
                .map(|x| x.repr())
                .collect::<Vec<String>>()
                .join(", ");

            format!("fn ({}) {}", params, self.body.repr())
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Call {
        callable: Box<Expression>,
        params: Vec<Expression>,
    }

    impl Call {
        pub fn new(callable: Expression, params: &Vec<Expression>) -> Call {
            Call {
                callable: Box::new(callable),
                params: params.to_vec(),
            }
        }
    }

    impl Representable for Call {
        fn repr(&self) -> String {
            let params = self
                .params
                .iter()
                .map(|x| x.repr())
                .collect::<Vec<String>>()
                .join(", ");

            format!("{}({})", self.callable.repr(), params)
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

    #[derive(Clone, Debug, PartialEq)]
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

            if let Some(Token::Assign) = p.peek_token() {
                // Moving on "="
                p.next_token();
                // Moving to token after "="
                p.next_token();
            } else {
                return Err(format!("Expected Assign token",));
            }

            let value = p.parse_expression_lowest()?;

            // Skipping semicolumn
            if let Some(Token::Semicolon) = p.peek_token() {
                p.next_token();
            }

            Ok(Statement::Let(Let::new(Identifier::new(ident), value)))
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Return {
        expr: Expression,
    }

    impl Return {
        pub fn new(expr: Expression) -> Return {
            Return { expr: expr }
        }
    }

    impl Representable for Return {
        fn repr(&self) -> String {
            format!("return {};", self.expr.repr())
        }
    }

    impl StatementType for Return {
        fn parse(p: &mut parser::Parser) -> Result<Statement, String> {
            // Moving to token after "return" keyword
            p.next_token();
            let expr = p.parse_expression_lowest()?;

            if let Some(Token::Semicolon) = p.peek_token() {
                p.next_token();
            }

            Ok(Statement::Return(Return::new(expr)))
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct ExpressionStatement {
        expr: Expression,
    }

    impl Representable for ExpressionStatement {
        fn repr(&self) -> String {
            format!("{}", self.expr.repr())
        }
    }

    impl ExpressionStatement {
        pub fn new(expr: Expression) -> ExpressionStatement {
            ExpressionStatement { expr: expr }
        }

        pub fn expr(&self) -> Expression {
            self.expr.clone()
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

    #[derive(Clone, Debug, PartialEq)]
    pub struct Block {
        statements: Vec<Statement>,
    }

    impl Block {
        pub fn new(statements: Vec<Statement>) -> Block {
            Block {
                statements: statements,
            }
        }
    }

    impl Representable for Block {
        fn repr(&self) -> String {
            let body_opt = self.statements.iter().fold(None, |acc, s| match acc {
                None => Some(format!("\n{}", s.repr())),
                Some(acc_s) => Some(format!("{};\n{}", acc_s, s.repr())),
            });
            if let Some(body) = body_opt {
                // equivalent to {<body>}
                format!("{{{}\n}}", body)
            } else {
                "".to_string()
            }
        }
    }

    impl StatementType for Block {
        fn parse(p: &mut parser::Parser) -> Result<Statement, String> {
            let mut statements = Vec::<Statement>::new();
            loop {
                match p.next_token() {
                    Some(Token::RightBrace) | None => {
                        break;
                    }
                    Some(tok) => {
                        let stmt = p.parse_statement(&tok)?;
                        statements.push(stmt);
                    }
                }
            }
            Ok(Statement::Block(Block::new(statements)))
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
        test_repr("toto;", "toto");
    }
}
