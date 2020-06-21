use crate::ast::*;
use crate::object::Object;
use crate::token::Token;

fn eval_bang_prefix_expression(obj: Object) -> Result<Object, String> {
    match obj {
        Object::Bool(b) => Ok(Object::Bool(!b)),
        _ => Err(format!("Bang operator not supported for {:?}", obj)),
    }
}

fn eval_minus_prefix_expression(obj: Object) -> Result<Object, String> {
    match obj {
        Object::Int(i) => Ok(Object::Int(-i)),
        Object::Float(f) => Ok(Object::Float(-f)),
        _ => Err(format!("Minus operator not supported for {:?}", obj)),
    }
}

fn eval_prefix_expression(prefix: &expressions::Prefix) -> Result<Object, String> {
    let right = eval(prefix.expression().as_node())?;
    let token = prefix.token();
    match token {
        Token::Bang => eval_bang_prefix_expression(right),
        Token::Minus => eval_minus_prefix_expression(right),
        _ => Err(format!("Prefix '{}' not implemented", token.repr())),
    }
}

fn eval_expression(e: &Expression) -> Result<Object, String> {
    match e {
        Expression::Int(i) => Ok(Object::Int(i.value())),
        Expression::Float(f) => Ok(Object::Float(f.value())),
        Expression::Boolean(b) => Ok(Object::Bool(b.value())),
        Expression::Prefix(p) => eval_prefix_expression(p),
        _ => Err(format!("Expression type {:?} not implemented", e)),
    }
}

fn eval_statement(statement: &Statement) -> Result<Object, String> {
    match statement {
        Statement::ExpressionStatement(s) => eval_expression(&s.expr()),
        _ => Err(format!("Statement type {:?} not implemented", statement)),
    }
}

fn eval_statements(statements: &Vec<Statement>) -> Result<Object, String> {
    let mut result = Err("No statements".to_string());

    for statement in statements {
        result = eval(Node::Statement(statement.clone()));
    }
    result
}

pub fn eval(node: Node) -> Result<Object, String> {
    match node {
        Node::Program(p) => eval_statements(p.statements()),
        Node::Expression(e) => eval_expression(&e), //::Int(i) => Object::Int(i),
        Node::Statement(s) => eval_statement(&s),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn test_eval(input: String) -> Object {
        let mut lex = Lexer::new(&input).unwrap();
        let mut parser = Parser::new(&mut lex).unwrap();
        let program = parser.parse_program().unwrap();
        return eval(program.as_node()).unwrap();
    }

    fn test_multiple_eval(cases: Vec<(String, Object)>) {
        for case in cases {
            assert_eq!(test_eval(case.0), case.1);
        }
    }

    #[test]
    fn test_eval_int() {
        let res = test_eval("5".to_string());
        assert_eq!(res, Object::Int(5));
    }

    #[test]
    fn test_eval_bool() {
        test_multiple_eval(vec![
            ("true".to_string(), Object::Bool(true)),
            ("false".to_string(), Object::Bool(false)),
        ]);
    }

    #[test]
    fn test_eval_bang() {
        test_multiple_eval(vec![
            ("!true".to_string(), Object::Bool(false)),
            ("!false".to_string(), Object::Bool(true)),
            ("!!true".to_string(), Object::Bool(true)),
            ("!!false".to_string(), Object::Bool(false)),
        ]);
    }
    #[test]
    fn test_eval_minus() {
        test_multiple_eval(vec![
            // int
            ("-42".to_string(), Object::Int(-42)),
            ("-1337".to_string(), Object::Int(-1337)),
            ("--42".to_string(), Object::Int(42)),
            ("--1337".to_string(), Object::Int(1337)),
            // float
            ("-42.42".to_string(), Object::Float(-42.42)),
            ("-1337.1337".to_string(), Object::Float(-1337.1337)),
            ("--42.42".to_string(), Object::Float(42.42)),
            ("--1337.1337".to_string(), Object::Float(1337.1337)),
        ]);
    }
}
