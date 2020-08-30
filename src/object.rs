use crate::ast::{expressions, statements, Representable};
use crate::environment::Environment;
use std::cell::RefCell;
use std::cmp;
use std::fmt;
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

type BuiltInFunc = fn(&[Object]) -> Result<Object, String>;

#[derive(Clone)]
pub struct BuiltIn {
    fn_: BuiltInFunc,
    name: String,
}

impl BuiltIn {
    pub fn repr(&self) -> String {
        format!("<built-in '{}'>", self.name)
    }

    pub fn call(&self, args: &[Object]) -> Result<Object, String> {
        (self.fn_)(args)
    }

    pub fn new(name: String, fn_: BuiltInFunc) -> BuiltIn {
        BuiltIn { fn_, name }
    }
}

impl fmt::Debug for BuiltIn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.repr())
    }
}

impl cmp::PartialEq for BuiltIn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    elems: Vec<Object>,
}

impl Array {
    pub fn new(elems: &[Object]) -> Array {
        Array {
            elems: elems.to_vec(),
        }
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn head(&self) -> Object {
        match self.get(0) {
            Some(o) => o,
            None => Object::None,
        }
    }

    pub fn tail(&self) -> Object {
        Object::Array(Array::new(if self.elems.len() > 1 {
            &self.elems[1..]
        } else {
            &[]
        }))
    }

    pub fn get(&self, idx: usize) -> Option<Object> {
        match self.elems.get(idx) {
            Some(o) => Some(o.clone()),
            None => None,
        }
    }

    pub fn repr(&self) -> String {
        let elems = self
            .elems
            .iter()
            .map(|x| x.repr())
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{}]", elems)
    }

    pub fn index_access(&self, idx: i32) -> Result<Object, String> {
        match idx {
            _ if idx < 0 => Err("Negative indexes aren't supported".to_string()),
            _ if idx as usize >= self.elems.len() => Err("Index out of range".to_string()),
            _ => Ok(self.elems[idx as usize].clone()),
        }
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
    BuiltIn(BuiltIn),
    Array(Array),
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
            Object::BuiltIn(b) => b.repr(),
            Object::Array(a) => a.repr(),
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
            Object::BuiltIn(_) => true,
            Object::Array(a) => a.elems.len() > 0,
        }
    }
}
