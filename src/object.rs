use crate::ast::{expressions, statements, Representable};
use crate::environment::Environment;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    env: Rc<RefCell<Environment>>,
    fn_: expressions::Fn,
}

impl Function {
    pub fn new(
        env: &Rc<RefCell<Environment>>,
        params: &[expressions::Identifier],
        body: statements::Block,
    ) -> Function {
        Function {
            env: Rc::clone(&env),
            fn_: expressions::Fn::new(body, params),
        }
    }

    pub fn repr(&self) -> String {
        self.fn_.repr()
    }

    pub fn params(&self) -> Vec<expressions::Identifier> {
        self.fn_.params()
    }

    pub fn body(&self) -> statements::Block {
        self.fn_.body()
    }

    pub fn env(&self) -> &Rc<RefCell<Environment>> {
        &self.env
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i32),
    Float(f32),
    Bool(bool),
    Str(String),
    ReturnValue(Box<Object>),
    Function(Function),
    None,
}

impl Object {
    pub fn repr(&self) -> String {
        match self {
            Object::Int(i) => format!("{}", i),
            Object::Float(f) => format!("{}", f),
            Object::Str(s) => s.to_string(),
            Object::Bool(b) => format!("{}", b),
            Object::None => "None".to_string(),
            Object::ReturnValue(o) => o.repr(),
            Object::Function(f) => f.repr(),
        }
    }

    pub fn is_true(&self) -> bool {
        match self {
            Object::Int(i) => *i != 0,
            Object::Float(f) => *f != 0.0,
            Object::Bool(b) => *b,
            Object::Str(s) => !s.is_empty(),
            Object::None => false,
            Object::ReturnValue(o) => o.is_true(),
            Object::Function(_) => true,
        }
    }
}
