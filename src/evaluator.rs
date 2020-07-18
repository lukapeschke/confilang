use crate::ast::*;
use crate::environment::Environment;
use crate::object::Object;
use crate::token::Token;
use crate::token::Token::*;
use std::ops::{Add, Div, Mul, Sub};

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Environment::new(),
        }
    }

    fn eval_bang_prefix_expression(&self, obj: Object) -> Result<Object, String> {
        match obj {
            Object::Bool(b) => Ok(Object::Bool(!b)),
            _ => Err(format!("Bang operator not supported for {:?}", obj)),
        }
    }

    fn eval_minus_prefix_expression(&self, obj: Object) -> Result<Object, String> {
        match obj {
            Object::Int(i) => Ok(Object::Int(-i)),
            Object::Float(f) => Ok(Object::Float(-f)),
            _ => Err(format!("Minus operator not supported for {:?}", obj)),
        }
    }

    fn eval_prefix_expression(&mut self, prefix: &expressions::Prefix) -> Result<Object, String> {
        let right = self.eval(prefix.expression().as_node())?;
        let token = prefix.token();
        match token {
            Bang => self.eval_bang_prefix_expression(right),
            Minus => self.eval_minus_prefix_expression(right),
            _ => Err(format!("Prefix '{}' not implemented", token.repr())),
        }
    }

    fn basic_bool_op<T>(&self, left: T, right: T, token: &Token) -> Result<bool, String>
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

    fn basic_arithmetic_op<T>(&self, left: T, right: T, token: &Token) -> Result<T, String>
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

    fn float_op(&self, left: f32, right: f32, token: &Token) -> Result<Object, String> {
        match token {
            &Plus | &Minus | &Asterisk | &Slash => {
                Ok(Object::Float(self.basic_arithmetic_op(left, right, token)?))
            }
            &Equals | &Differs | &Lt | &Gt | &Le | &Ge => {
                Ok(Object::Bool(self.basic_bool_op(left, right, token)?))
            }
            _ => Err(format!("Token '{}' not supported for Int", token.repr())),
        }
    }

    fn int_op(&self, left: i32, right: i32, token: &Token) -> Result<Object, String> {
        // TODO: Add support for modulo
        match token {
            &Plus | &Minus | &Asterisk | &Slash => {
                Ok(Object::Int(self.basic_arithmetic_op(left, right, token)?))
            }
            &Equals | &Differs | &Lt | &Gt | &Le | &Ge => {
                Ok(Object::Bool(self.basic_bool_op(left, right, token)?))
            }
            _ => Err(format!("Token '{}' not supported for Int", token.repr())),
        }
    }

    fn eval_numeric_infix_expression(
        &self,
        left: &Object,
        right: &Object,
        token: &Token,
    ) -> Result<Object, String> {
        match (left, right) {
            (&Object::Int(l), &Object::Int(r)) => self.int_op(l, r, token),
            (&Object::Float(l), &Object::Int(r)) => self.float_op(l, r as f32, token),
            (&Object::Int(l), &Object::Float(r)) => self.float_op(l as f32, r, token),
            (&Object::Float(l), &Object::Float(r)) => self.float_op(l, r, token),
            _ => Err(format!(
                "Objects {:?} and {:?} not supported for arithmetic operations",
                left, right,
            )),
        }
    }

    fn eval_infix_expression(&mut self, infix: &expressions::Infix) -> Result<Object, String> {
        let right = &self.eval(infix.right().as_node())?;
        let left = &self.eval(infix.left().as_node())?;
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
                self.eval_numeric_infix_expression(&left, &right, &token)
            }
            _ => Err(format!("Infix '{}' not implemented", infix.repr())),
        }
    }

    fn eval_if_expression(&mut self, if_: &expressions::If) -> Result<Object, String> {
        let cond = self.eval(if_.condition().as_node())?;
        if cond.is_true() {
            self.eval(if_.consequence().as_node())
        } else if let Some(alt) = if_.alternative() {
            self.eval(alt.as_node())
        } else {
            Ok(Object::None)
        }
    }

    fn eval_identifier(&mut self, id: &expressions::Identifier) -> Result<Object, String> {
        let key = id.name();
        if let Some(obj) = self.env.get(&key) {
            return Ok(obj);
        }
        Err(format!("Identifier '{}' not found", key))
    }

    fn eval_expression(&mut self, e: &Expression) -> Result<Object, String> {
        match e {
            Expression::Int(i) => Ok(Object::Int(i.value())),
            Expression::Float(f) => Ok(Object::Float(f.value())),
            Expression::Boolean(b) => Ok(Object::Bool(b.value())),
            Expression::Prefix(p) => self.eval_prefix_expression(p),
            Expression::Infix(i) => self.eval_infix_expression(i),
            Expression::If(if_) => self.eval_if_expression(if_),
            Expression::Identifier(id) => self.eval_identifier(id),
            _ => Err(format!("Expression type {:?} not implemented", e)),
        }
    }
    fn eval_let_statement(&mut self, s: &statements::Let) -> Result<Object, String> {
        let val = self.eval(s.value().as_node())?;
        self.env.set(&s.ident().name(), val);
        Ok(Object::None)
    }

    fn eval_statement(&mut self, statement: &Statement) -> Result<Object, String> {
        match statement {
            Statement::ExpressionStatement(s) => self.eval_expression(&s.expr()),
            Statement::Block(s) => self.eval_block_statement(&s.statements()),
            Statement::Return(r) => Ok(Object::ReturnValue(Box::new(
                self.eval_expression(&r.expr())?,
            ))),
            Statement::Let(l) => self.eval_let_statement(l),
        }
    }

    fn eval_block_statement(&mut self, statements: &[Statement]) -> Result<Object, String> {
        let mut result = Err("No statements".to_string());

        for statement in statements {
            result = Ok(self.eval(Node::Statement(statement.clone()))?);
            if let Ok(Object::ReturnValue(o)) = result {
                // Allows to break all nested return statements, whereas
                // eval_program only breaks one level (we lose the information
                // that the value was returned)
                return Ok(Object::ReturnValue(o));
            }
        }
        result
    }

    fn eval_program(&mut self, statements: &[Statement]) -> Result<Object, String> {
        let mut result = Err("No statements".to_string());

        for statement in statements {
            result = Ok(self.eval(Node::Statement(statement.clone()))?);
            if let Ok(Object::ReturnValue(o)) = result {
                return Ok(*o);
            }
        }
        result
    }

    pub fn eval(&mut self, node: Node) -> Result<Object, String> {
        match node {
            Node::Program(p) => self.eval_program(p.statements()),
            Node::Expression(e) => self.eval_expression(&e),
            Node::Statement(s) => self.eval_statement(&s),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn eval(node: Node) -> Result<Object, String> {
        Evaluator::new().eval(node)
    }

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

    #[test]
    fn test_eval_return() {
        test_multiple_eval(vec![
            ("return 10;".to_string(), Object::Int(10)),
            ("return 10; 9;".to_string(), Object::Int(10)),
            ("return 2 * 5; 9;".to_string(), Object::Int(10)),
            ("9; return 2 * 5; 9;".to_string(), Object::Int(10)),
            (
                "if (10 > 1) {if (10 > 1) {return 10;} return 1;}".to_string(),
                Object::Int(10),
            ),
        ])
    }
    #[test]
    fn test_eval_let() {
        test_multiple_eval(vec![
            ("let a = 5; a;".to_string(), Object::Int(5)),
            ("let a = 5 * 5; a;".to_string(), Object::Int(25)),
            ("let a = 5; let b = a; b;".to_string(), Object::Int(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;".to_string(),
                Object::Int(15),
            ),
        ])
    }
}
