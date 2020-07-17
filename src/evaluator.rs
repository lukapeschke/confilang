use crate::ast::*;
use crate::object::Object;
use crate::token::Token;
use crate::token::Token::*;
use std::ops::{Add, Div, Mul, Sub};

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
        Bang => eval_bang_prefix_expression(right),
        Minus => eval_minus_prefix_expression(right),
        _ => Err(format!("Prefix '{}' not implemented", token.repr())),
    }
}

fn basic_bool_op<T>(left: T, right: T, token: &Token) -> Result<bool, String>
where
    T: PartialEq + PartialOrd,
{
    match *token {
        Equals => Ok(left == right),
        Differs => Ok(left != right),
        Lt => Ok(left < right),
        Gt => Ok(left > right),
        Le => Ok(left <= right),
        Ge => Ok(left >= right),

        _ => Err("Basic bool op not implemented".to_string()),
    }
}

fn basic_arithmetic_op<T>(left: T, right: T, token: &Token) -> Result<T, String>
where
    T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T> + Copy + Clone,
{
    match *token {
        Plus => Ok(left + right),
        Minus => Ok(left - right),
        Asterisk => Ok(left * right),
        Slash => Ok(left / right),
        _ => Err("Basic op not implemented".to_string()),
    }
}

fn float_op(left: f32, right: f32, token: &Token) -> Result<Object, String> {
    match token {
        &Plus | &Minus | &Asterisk | &Slash => {
            Ok(Object::Float(basic_arithmetic_op(left, right, token)?))
        }
        &Equals | &Differs | &Lt | &Gt | &Le | &Ge => {
            Ok(Object::Bool(basic_bool_op(left, right, token)?))
        }
        _ => Err(format!("Token '{}' not supported for Int", token.repr())),
    }
}

fn int_op(left: i32, right: i32, token: &Token) -> Result<Object, String> {
    // TODO: Add support for modulo
    match token {
        &Plus | &Minus | &Asterisk | &Slash => {
            Ok(Object::Int(basic_arithmetic_op(left, right, token)?))
        }
        &Equals | &Differs | &Lt | &Gt | &Le | &Ge => {
            Ok(Object::Bool(basic_bool_op(left, right, token)?))
        }
        _ => Err(format!("Token '{}' not supported for Int", token.repr())),
    }
}

fn eval_numeric_infix_expression(
    left: &Object,
    right: &Object,
    token: &Token,
) -> Result<Object, String> {
    match (left, right) {
        (&Object::Int(l), &Object::Int(r)) => int_op(l, r, token),
        (&Object::Float(l), &Object::Int(r)) => float_op(l, r as f32, token),
        (&Object::Int(l), &Object::Float(r)) => float_op(l as f32, r, token),
        (&Object::Float(l), &Object::Float(r)) => float_op(l, r, token),
        _ => Err(format!(
            "Objects {:?} and {:?} not supported for arithmetic operations",
            left, right,
        )),
    }
}

fn eval_infix_expression(infix: &expressions::Infix) -> Result<Object, String> {
    let right = eval(infix.right().as_node())?;
    let left = eval(infix.left().as_node())?;
    let token = infix.token();

    match (&left, &right) {
        // https://github.com/rust-lang/rust/issues/54883
        // (Object::Int(_) | Object::Float(_), Object::Int(_) | Object::Float(_)) => {
        //     eval_arithmetic_infix_expression(left, right, token)
        // }
        (&Object::Int(_), &Object::Int(_))
        | (&Object::Int(_), &Object::Float(_))
        | (&Object::Float(_), &Object::Int(_))
        | (&Object::Float(_), &Object::Float(_)) => {
            eval_numeric_infix_expression(&left, &right, &token)
        }
        _ => Err(format!("Infix '{}' not implemented", infix.repr())),
    }
}

fn eval_if_expression(if_: &expressions::If) -> Result<Object, String> {
    let cond = eval(if_.condition().as_node())?;
    if cond.is_true() {
        eval(if_.consequence().as_node())
    } else if let Some(alt) = if_.alternative() {
        eval(alt.as_node())
    } else {
        Ok(Object::None)
    }
}

fn eval_expression(e: &Expression) -> Result<Object, String> {
    match e {
        Expression::Int(i) => Ok(Object::Int(i.value())),
        Expression::Float(f) => Ok(Object::Float(f.value())),
        Expression::Boolean(b) => Ok(Object::Bool(b.value())),
        Expression::Prefix(p) => eval_prefix_expression(p),
        Expression::Infix(i) => eval_infix_expression(i),
        Expression::If(if_) => eval_if_expression(if_),
        _ => Err(format!("Expression type {:?} not implemented", e)),
    }
}

fn eval_statement(statement: &Statement) -> Result<Object, String> {
    match statement {
        Statement::ExpressionStatement(s) => eval_expression(&s.expr()),
        Statement::Block(s) => eval_statements(&s.statements()),
        _ => Err(format!("Statement type {:?} not implemented", statement)),
    }
}

fn eval_statements(statements: &[Statement]) -> Result<Object, String> {
    let mut result = Err("No statements".to_string());

    for statement in statements {
        result = eval(Node::Statement(statement.clone()));
    }
    result
}

pub fn eval(node: Node) -> Result<Object, String> {
    match node {
        Node::Program(p) => eval_statements(p.statements()),
        Node::Expression(e) => eval_expression(&e),
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

    #[test]
    fn test_eval_int_op() {
        test_multiple_eval(vec![
            ("5".to_string(), Object::Int(5)),
            ("10".to_string(), Object::Int(10)),
            ("-5".to_string(), Object::Int(-5)),
            ("-10".to_string(), Object::Int(-10)),
            ("5 + 5 + 5 + 5 - 10".to_string(), Object::Int(10)),
            ("2 * 2 * 2 * 2 * 2".to_string(), Object::Int(32)),
            ("-50 + 100 + -50".to_string(), Object::Int(0)),
            ("5 * 2 + 10".to_string(), Object::Int(20)),
            ("5 + 2 * 10".to_string(), Object::Int(25)),
            ("20 + 2 * -10".to_string(), Object::Int(0)),
            ("50 / 2 * 2 + 10".to_string(), Object::Int(60)),
            ("2 * (5 + 10)".to_string(), Object::Int(30)),
            ("3 * 3 * 3 + 10".to_string(), Object::Int(37)),
            ("3 * (3 * 3) + 10".to_string(), Object::Int(37)),
            (
                "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
                Object::Int(50),
            ),
        ]);
    }

    #[test]
    fn test_eval_float_op() {
        test_multiple_eval(vec![
            ("5.0".to_string(), Object::Float(5.0)),
            ("10.0".to_string(), Object::Float(10.0)),
            ("-5.0".to_string(), Object::Float(-5.0)),
            ("-10.0".to_string(), Object::Float(-10.0)),
            ("5 + 5.0 + 5 + 5 - 10".to_string(), Object::Float(10.0)),
            ("2 * 2.0 * 2 * 2 * 2".to_string(), Object::Float(32.0)),
            ("-50.0 + 100 + -50".to_string(), Object::Float(0.0)),
            ("5 * 2 + 10.0".to_string(), Object::Float(20.0)),
            ("5 + 2.0 * 10".to_string(), Object::Float(25.0)),
            ("20.0 + 2 * -10".to_string(), Object::Float(0.0)),
            ("50 / 2.0 * 2 + 10".to_string(), Object::Float(60.0)),
            ("2 * (5 + 10.0)".to_string(), Object::Float(30.0)),
            ("3 * 3 * 3.0 + 10".to_string(), Object::Float(37.0)),
            ("3 * (3 * 3.0) + 10".to_string(), Object::Float(37.0)),
            (
                "(5 + 10.0 * 2 + 15 / 3) * 2 + -10".to_string(),
                Object::Float(50.0),
            ),
        ]);
    }

    #[test]
    fn test_eval_bool_op() {
        test_multiple_eval(vec![
            ("true".to_string(), Object::Bool(true)),
            ("false".to_string(), Object::Bool(false)),
            ("1 < 2".to_string(), Object::Bool(true)),
            ("1 > 2".to_string(), Object::Bool(false)),
            ("1 < 1".to_string(), Object::Bool(false)),
            ("1 > 1".to_string(), Object::Bool(false)),
            ("1 == 1".to_string(), Object::Bool(true)),
            ("1 != 1".to_string(), Object::Bool(false)),
            ("1 == 2".to_string(), Object::Bool(false)),
            ("1 != 2".to_string(), Object::Bool(true)),
            ("true".to_string(), Object::Bool(true)),
            ("false".to_string(), Object::Bool(false)),
            ("1.0 < 2.0".to_string(), Object::Bool(true)),
            ("1 > 2.0".to_string(), Object::Bool(false)),
            ("1 < 1.0".to_string(), Object::Bool(false)),
            ("1 > 1.0".to_string(), Object::Bool(false)),
            ("1 == 1.0".to_string(), Object::Bool(true)),
            ("1 != 1".to_string(), Object::Bool(false)),
            ("1 == 2".to_string(), Object::Bool(false)),
            ("1 != 2".to_string(), Object::Bool(true)),
        ]);
    }

    #[test]
    fn test_eval_conditional() {
        test_multiple_eval(vec![
            ("if (true) { 10 }".to_string(), Object::Int(10)),
            ("if (false) { 10 }".to_string(), Object::None),
            ("if (1) { 10 }".to_string(), Object::Int(10)),
            ("if (1 < 2) { 10 }".to_string(), Object::Int(10)),
            ("if (1 > 2) { 10 }".to_string(), Object::None),
            ("if (1 > 2) { 10 } else {42}".to_string(), Object::Int(42)),
        ])
    }
}
