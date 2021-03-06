use crate::ast::*;
use crate::builtins;
use crate::environment::Environment;
use crate::object::Array;
use crate::object::Function;
use crate::object::HashMap;
use crate::object::Object;
use crate::token::Token;
use crate::token::Token::*;
use crate::utils::HashableFloat;
use std::cell::RefCell;
use std::ops::{Add, Div, Mul, Sub};
use std::rc::Rc;
pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Rc::new(RefCell::new(Environment::new(None))),
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

    fn eval_prefix_expression(
        &mut self,
        prefix: &expressions::Prefix,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let right = self.eval(prefix.expression().as_node(), env)?;
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
            &Plus | &Minus | &Asterisk | &Slash => Ok(Object::Float(HashableFloat::new(
                self.basic_arithmetic_op(left, right, token)?,
            ))),
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
        match (&left, &right) {
            (&Object::Int(l), &Object::Int(r)) => self.int_op(*l, *r, token),
            (&Object::Float(l), &Object::Int(r)) => self.float_op(l.value(), *r as f32, token),
            (&Object::Int(l), &Object::Float(r)) => self.float_op(*l as f32, r.value(), token),
            (&Object::Float(l), &Object::Float(r)) => self.float_op(l.value(), r.value(), token),
            _ => Err(format!(
                "Objects {:?} and {:?} not supported for arithmetic operations",
                left, right,
            )),
        }
    }

    fn eval_str_infix_expression(
        &self,
        left: &str,
        right: &str,
        token: &Token,
    ) -> Result<Object, String> {
        match token {
            Token::Plus => Ok(Object::Str(format!("{}{}", left, right))),
            _ => Err(format!("Unsupported infix token {} for str", token.repr())),
        }
    }

    fn eval_infix_expression(
        &mut self,
        infix: &expressions::Infix,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let right = &self.eval(infix.right().as_node(), env)?;
        let left = &self.eval(infix.left().as_node(), env)?;
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
            (&Object::Str(l), &Object::Str(r)) => self.eval_str_infix_expression(l, r, &token),
            _ => Err(format!("Infix '{}' not implemented", infix.repr())),
        }
    }

    fn eval_if_expression(
        &mut self,
        if_: &expressions::If,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let cond = self.eval(if_.condition().as_node(), env)?;
        if cond.is_true() {
            self.eval(if_.consequence().as_node(), env)
        } else if let Some(alt) = if_.alternative() {
            self.eval(alt.as_node(), env)
        } else {
            Ok(Object::None)
        }
    }

    fn eval_identifier(
        &mut self,
        id: &expressions::Identifier,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let local_env = match env {
            Some(ev) => ev,
            None => &self.env,
        };
        let key = id.name();
        if let Some(obj) = local_env.borrow().get(&key) {
            return Ok(obj);
        }
        if let Some(func) = builtins::get(&key) {
            return Ok(Object::BuiltIn(func));
        }
        Err(format!("Identifier '{}' not found", key))
    }

    fn eval_fn(&mut self, fn_: &expressions::Fn, env: &Option<Rc<RefCell<Environment>>>) -> Object {
        let local_env = match env {
            Some(ev) => ev,
            None => &self.env,
        };
        Object::Function(Function::new(local_env, &fn_.params(), fn_.body()))
    }

    fn eval_expressions(
        &mut self,
        expressions: &[Expression],
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Vec<Object>, String> {
        let mut output = Vec::new();
        for exp in expressions.iter() {
            output.push(self.eval_expression(exp, env)?)
        }
        Ok(output)
    }

    fn new_fn_env(&self, fn_: &Function, args: &[Object]) -> Environment {
        let mut env = Environment::new(Some(fn_.env()));
        let arg_names = fn_.params();
        for (idx, arg) in args.iter().enumerate() {
            env.set(&arg_names[idx].name(), arg.clone());
        }
        env
    }

    fn eval_call(
        &mut self,
        c: &expressions::Call,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let obj = self.eval(c.callable().as_node(), env)?;
        let args = self.eval_expressions(&c.params(), env)?;
        match obj {
            Object::Function(fn_) => {
                let fn_env = Rc::new(RefCell::new(self.new_fn_env(&fn_, &args)));
                let evaluated =
                    self.eval_block_statement(&fn_.body().statements(), &Some(fn_env))?;
                Ok(match evaluated {
                    // Here we "unwrap" the ReturnValue, so that we don't break
                    // several functions
                    Object::ReturnValue(o) => *o,
                    _ => evaluated,
                })
            }
            Object::BuiltIn(b) => b.call(&args),
            _ => Err(format!("Object '{:?}' is not a function", obj)),
        }
    }

    fn eval_array(
        &mut self,
        a: &expressions::Array,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        Ok(Object::Array(Array::new(
            &self.eval_expressions(&a.elements(), env)?,
        )))
    }

    fn eval_index(
        &mut self,
        i: &expressions::Index,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let index_obj = self.eval(i.index().as_node(), env)?;
        let left_obj = self.eval(i.left().as_node(), env)?;
        match left_obj {
            Object::Array(a) => {
                if let Object::Int(idx) = index_obj {
                    a.index_access(idx)
                } else {
                    Err(format!(
                        "Array index must be an integer, got {}",
                        index_obj.repr()
                    ))
                }
            }
            Object::HashMap(h) => h.index_access(&index_obj),
            _ => Err(format!(
                "Index-based access not supported for {:?}",
                left_obj
            )),
        }
    }

    fn eval_hashmap(
        &mut self,
        h: &expressions::HashMap,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let mut output = std::collections::HashMap::new();

        let handle_k_v = |(k, v): (&Expression, &Expression)| -> Result<(), String> {
            let key = self.eval_expression(k, env)?;
            if !key.is_hashable() {
                return Err(format!("Unhashable object '{:?}'", key));
            }
            let val = self.eval_expression(v, env)?;
            output.insert(key, val);
            Ok(())
        };
        h.elems()
            .iter()
            .map(handle_k_v)
            .collect::<Result<(), String>>()?;

        Ok(Object::HashMap(HashMap::new(output)))
    }

    fn eval_expression(
        &mut self,
        e: &Expression,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        match e {
            Expression::Int(i) => Ok(Object::Int(i.value())),
            Expression::Float(f) => Ok(Object::Float(HashableFloat::new(f.value()))),
            Expression::Str(s) => Ok(Object::Str(s.value())),
            Expression::Boolean(b) => Ok(Object::Bool(b.value())),
            Expression::Prefix(p) => self.eval_prefix_expression(p, env),
            Expression::Infix(i) => self.eval_infix_expression(i, env),
            Expression::If(if_) => self.eval_if_expression(if_, env),
            Expression::Identifier(id) => self.eval_identifier(id, env),
            Expression::Fn(f) => Ok(self.eval_fn(f, env)),
            Expression::Call(c) => self.eval_call(c, env),
            Expression::Array(a) => self.eval_array(a, env),
            Expression::Index(i) => self.eval_index(i, env),
            Expression::HashMap(h) => self.eval_hashmap(h, env),
        }
    }

    fn eval_let_statement(
        &mut self,
        s: &statements::Let,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let val = self.eval(s.value().as_node(), env)?;
        self.env.borrow_mut().set(&s.ident().name(), val);
        Ok(Object::None)
    }

    fn eval_statement(
        &mut self,
        statement: &Statement,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        match statement {
            Statement::ExpressionStatement(s) => self.eval_expression(&s.expr(), env),
            Statement::Block(s) => self.eval_block_statement(&s.statements(), env),
            Statement::Return(r) => Ok(Object::ReturnValue(Box::new(
                self.eval_expression(&r.expr(), env)?,
            ))),
            Statement::Let(l) => self.eval_let_statement(l, env),
        }
    }

    fn eval_block_statement(
        &mut self,
        statements: &[Statement],
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let mut result = Err("No statements".to_string());

        for statement in statements {
            result = Ok(self.eval(Node::Statement(statement.clone()), env)?);
            if let Ok(Object::ReturnValue(o)) = result {
                // Allows to break all nested return statements, whereas
                // eval_program only breaks one level (we lose the information
                // that the value was returned)
                return Ok(Object::ReturnValue(o));
            }
        }
        result
    }

    fn eval_program(
        &mut self,
        statements: &[Statement],
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        let mut result = Err("No statements".to_string());

        for statement in statements {
            result = Ok(self.eval(Node::Statement(statement.clone()), env)?);
            if let Ok(Object::ReturnValue(o)) = result {
                return Ok(*o);
            }
        }
        result
    }

    pub fn eval(
        &mut self,
        node: Node,
        env: &Option<Rc<RefCell<Environment>>>,
    ) -> Result<Object, String> {
        match node {
            Node::Program(p) => self.eval_program(p.statements(), env),
            Node::Expression(e) => self.eval_expression(&e, env),
            Node::Statement(s) => self.eval_statement(&s, env),
        }
    }
}

#[cfg(test)]
pub mod tests {

    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn eval(node: Node) -> Result<Object, String> {
        Evaluator::new().eval(node, &None)
    }

    fn test_eval(input: String) -> Object {
        let mut lex = Lexer::new(&input).unwrap();
        let mut parser = Parser::new(&mut lex).unwrap();
        let program = parser.parse_program().unwrap();
        return eval(program.as_node()).unwrap();
    }

    pub fn test_multiple_eval(cases: Vec<(String, Object)>) {
        for case in cases {
            println!("Testing with {}", case.0);
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
            (
                "-42.42".to_string(),
                Object::Float(HashableFloat::new(-42.42)),
            ),
            (
                "-1337.1337".to_string(),
                Object::Float(HashableFloat::new(-1337.1337)),
            ),
            (
                "--42.42".to_string(),
                Object::Float(HashableFloat::new(42.42)),
            ),
            (
                "--1337.1337".to_string(),
                Object::Float(HashableFloat::new(1337.1337)),
            ),
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
            ("5.0".to_string(), Object::Float(HashableFloat::new(5.0))),
            ("10.0".to_string(), Object::Float(HashableFloat::new(10.0))),
            ("-5.0".to_string(), Object::Float(HashableFloat::new(-5.0))),
            (
                "-10.0".to_string(),
                Object::Float(HashableFloat::new(-10.0)),
            ),
            (
                "5 + 5.0 + 5 + 5 - 10".to_string(),
                Object::Float(HashableFloat::new(10.0)),
            ),
            (
                "2 * 2.0 * 2 * 2 * 2".to_string(),
                Object::Float(HashableFloat::new(32.0)),
            ),
            (
                "-50.0 + 100 + -50".to_string(),
                Object::Float(HashableFloat::new(0.0)),
            ),
            (
                "5 * 2 + 10.0".to_string(),
                Object::Float(HashableFloat::new(20.0)),
            ),
            (
                "5 + 2.0 * 10".to_string(),
                Object::Float(HashableFloat::new(25.0)),
            ),
            (
                "20.0 + 2 * -10".to_string(),
                Object::Float(HashableFloat::new(0.0)),
            ),
            (
                "50 / 2.0 * 2 + 10".to_string(),
                Object::Float(HashableFloat::new(60.0)),
            ),
            (
                "2 * (5 + 10.0)".to_string(),
                Object::Float(HashableFloat::new(30.0)),
            ),
            (
                "3 * 3 * 3.0 + 10".to_string(),
                Object::Float(HashableFloat::new(37.0)),
            ),
            (
                "3 * (3 * 3.0) + 10".to_string(),
                Object::Float(HashableFloat::new(37.0)),
            ),
            (
                "(5 + 10.0 * 2 + 15 / 3) * 2 + -10".to_string(),
                Object::Float(HashableFloat::new(50.0)),
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

    #[test]
    fn test_eval_str() {
        test_multiple_eval(vec![
            (
                "let a = \"hello\"; a;".to_string(),
                Object::Str("hello".to_string()),
            ),
            (
                "\"hello\" + \" \" + \"world\"".to_string(),
                Object::Str("hello world".to_string()),
            ),
        ])
    }

    #[test]
    fn test_eval_fn() {
        test_multiple_eval(vec![
            (
                "let a = 5; let b = fn() { return a * 2 }; b()".to_string(),
                Object::Int(10),
            ),
            (
                "let fac = fn(x) {if (x < 1) {return x} return x + fac(x - 1)}; fac(3)".to_string(),
                Object::Int(6),
            ),
            (
                "let a = 5; let b = fn(a) { return a * 2 }; b(10)".to_string(),
                Object::Int(20),
            ),
            (
                "let a = 5; let b = fn(a) { return a * 2 }; b(10); a".to_string(),
                Object::Int(5),
            ),
            (
                "let identity = fn(x) { x; }; identity(5);".to_string(),
                Object::Int(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);".to_string(),
                Object::Int(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);".to_string(),
                Object::Int(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);".to_string(),
                Object::Int(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));".to_string(),
                Object::Int(20),
            ),
            ("fn(x) { x; }(5)".to_string(), Object::Int(5)),
        ])
    }

    #[test]
    fn test_eval_array() {
        test_multiple_eval(vec![(
            "let a = [1, 1+1, 2* 3, 8]; a".to_string(),
            Object::Array(Array::new(&vec![
                Object::Int(1),
                Object::Int(2),
                Object::Int(6),
                Object::Int(8),
            ])),
        )])
    }

    #[test]
    fn test_eval_array_index() {
        test_multiple_eval(vec![
            (
                "let a = [1, 1+1, 2* 3, 8]; a[2]".to_string(),
                Object::Int(6),
            ),
            (
                "let a = [1, 1+1, 2* 3, 8]; a[0]".to_string(),
                Object::Int(1),
            ),
        ])
    }

    #[test]
    fn test_eval_array_advanced() {
        test_multiple_eval(vec![
            (
                "
let map = fn(arr, f) {
    let iter = fn(arr, acc) {
        if len(arr) == 0 {
            acc
        } else {
            iter(tail(arr), append(acc, f(head(arr))))
        }
    };
    iter(arr, [])
};
let increment = fn(x) {x + 1};
map([0, 1, 2, 3], increment)
"
                .to_string(),
                Object::Array(Array::new(&[
                    Object::Int(1),
                    Object::Int(2),
                    Object::Int(3),
                    Object::Int(4),
                ])),
            ),
            (
                "
let fold = fn(arr, f, init) {
    let iter = fn(arr, acc) {
        if len(arr) == 0 {
            acc
        } else {
            iter(tail(arr), f(acc, head(arr)))
        }
    };
    iter(arr, init)
};
let add = fn(x, y) {x + y};
let sum = fn(arr) {fold(arr, add, 0)};
sum([0, 1, 2, 3])
"
                .to_string(),
                Object::Int(6),
            ),
        ])
    }

    #[test]
    fn test_eval_hashmap() {
        let mut res1 = std::collections::HashMap::new();
        res1.insert(Object::Int(1), Object::Int(2));
        res1.insert(
            Object::Str("hello".to_string()),
            Object::Str("hi".to_string()),
        );
        test_multiple_eval(vec![
            (
                "let a = {1: 2, \"hello\": \"hi\"}; a".to_string(),
                Object::HashMap(HashMap::new(res1)),
            ),
            (
                "let a = {true: false, false: false}; a[true]".to_string(),
                Object::Bool(false),
            ),
            (
                "let a = {true: 1, false: 2}; a[true] + a[false]".to_string(),
                Object::Int(3),
            ),
        ])
    }
}
