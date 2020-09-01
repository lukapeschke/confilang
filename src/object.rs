use crate::ast::{expressions, statements, Representable};
use crate::environment::Environment;
use crate::utils::HashableFloat;
use std::cell::RefCell;
use std::cmp;
use std::fmt;
use std::hash;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
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

// NOTE: Implementing Hash but deriving PartialEq is generally a bad idea since
// the implementations must agree, but the hash implementation panics
// anyway so ¯\_(ツ)_/¯
// https://rust-lang.github.io/rust-clippy/master/index.html#derive_hash_xor_eq
#[allow(clippy::derive_hash_xor_eq)]
impl hash::Hash for Function {
    fn hash<H: hash::Hasher>(&self, _state: &mut H) {
        panic!("hash not implemented for Function");
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

impl cmp::Eq for BuiltIn {}

// NOTE: Implementing Hash but deriving PartialEq is generally a bad idea since
// the implementations must agree, but the hash implementation panics
// anyway so ¯\_(ツ)_/¯
// https://rust-lang.github.io/rust-clippy/master/index.html#derive_hash_xor_eq
#[allow(clippy::derive_hash_xor_eq)]
impl hash::Hash for BuiltIn {
    fn hash<H: hash::Hasher>(&self, _state: &mut H) {
        panic!("hash not implemented for BuiltIn");
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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

    pub fn append(&self, obj: Object) -> Object {
        let mut vec = self.elems.clone();
        vec.push(obj);
        Object::Array(Array::new(&vec))
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HashMap {
    elems: std::collections::HashMap<Object, Object>,
}

impl HashMap {
    pub fn new(elems: std::collections::HashMap<Object, Object>) -> HashMap {
        HashMap { elems }
    }

    pub fn repr(&self) -> String {
        let pairs = self
            .elems
            .iter()
            .map(|(k, v)| format!("{}: {}", k.repr(), v.repr()))
            .collect::<Vec<String>>()
            .join(", ");
        // brackets + format brackets
        format!("{{{}}}", pairs)
    }

    pub fn index_access(&self, idx: &Object) -> Result<Object, String> {
        if !idx.is_hashable() {
            return Err(format!("Unhashable object {:?}", idx));
        }

        match self.elems.get(idx) {
            Some(obj) => Ok(obj.clone()),
            None => Err(format!("{} not in {}", idx.repr(), self.repr())),
        }
    }
}

// NOTE: Implementing Hash but deriving PartialEq is generally a bad idea since
// the implementations must agree, but the hash implementation panics
// anyway so ¯\_(ツ)_/¯
// https://rust-lang.github.io/rust-clippy/master/index.html#derive_hash_xor_eq
#[allow(clippy::derive_hash_xor_eq)]
impl hash::Hash for HashMap {
    fn hash<H: hash::Hasher>(&self, _state: &mut H) {
        panic!("hash not implemented for HashableMap");
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Object {
    Int(i32),
    Float(HashableFloat),
    Bool(bool),
    Str(String),
    ReturnValue(Box<Object>),
    Function(Function),
    BuiltIn(BuiltIn),
    Array(Array),
    HashMap(HashMap),
    None,
}

impl Object {
    pub fn repr(&self) -> String {
        match self {
            Object::Int(i) => format!("{}", i),
            Object::Float(f) => format!("{}", f.value()),
            Object::Str(s) => s.to_string(),
            Object::Bool(b) => format!("{}", b),
            Object::None => "None".to_string(),
            Object::ReturnValue(o) => o.repr(),
            Object::Function(f) => f.repr(),
            Object::BuiltIn(b) => b.repr(),
            Object::Array(a) => a.repr(),
            Object::HashMap(h) => h.repr(),
        }
    }

    pub fn is_true(&self) -> bool {
        match self {
            Object::Int(i) => *i != 0,
            Object::Float(f) => f.value() != 0.0,
            Object::Bool(b) => *b,
            Object::Str(s) => !s.is_empty(),
            Object::None => false,
            Object::ReturnValue(o) => o.is_true(),
            Object::Function(_) => true,
            Object::BuiltIn(_) => true,
            Object::Array(a) => !a.elems.is_empty(),
            Object::HashMap(h) => !h.elems.is_empty(),
        }
    }

    pub fn is_hashable(&self) -> bool {
        match self {
            Object::Int(_) | Object::Bool(_) | Object::Str(_) | Object::Array(_) => true,
            _ => false,
        }
    }
}
