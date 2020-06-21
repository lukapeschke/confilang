use crate::ast;
use crate::object::Object;

fn eval_expression(e: &ast::Expression) -> Result<Object, String> {
    match e {
        ast::Expression::Int(i) => Ok(Object::Int(i.value())),
        _ => Err(format!("Expression type {:?} not implemented", e)),
    }
}

fn eval_statement(statement: &ast::Statement) -> Result<Object, String> {
    match statement {
        ast::Statement::ExpressionStatement(s) => eval_expression(&s.expr()),
        _ => Err(format!("Statement type {:?} not implemented", statement)),
    }
}

fn eval_statements(statements: &Vec<ast::Statement>) -> Result<Object, String> {
    let mut result = Err("No statements".to_string());

    for statement in statements {
        result = eval(ast::Node::Statement(statement.clone()));
    }
    result
}

pub fn eval(node: ast::Node) -> Result<Object, String> {
    match node {
        ast::Node::Program(p) => eval_statements(p.statements()),
        ast::Node::Expression(e) => eval_expression(&e), //::Int(i) => Object::Int(i),
        ast::Node::Statement(s) => eval_statement(&s),
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
        return eval(ast::Node::Program(program)).unwrap();
    }

    #[test]
    fn test_eval_int() {
        let res = test_eval("5".to_string());
        println!("GOT {:?}", res);
        assert_eq!(res, Object::Int(5));
    }
}
