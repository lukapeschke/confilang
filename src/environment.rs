use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    map: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

// NOTE: Implementing Hash but deriving PartialEq is generally a bad idea since
// the implementations must agree, but the hash implementation panics
// anyway so ¯\_(ツ)_/¯
// https://rust-lang.github.io/rust-clippy/master/index.html#derive_hash_xor_eq
#[allow(clippy::derive_hash_xor_eq)]
impl hash::Hash for Environment {
    fn hash<H: hash::Hasher>(&self, _state: &mut H) {
        panic!("hash not implemented for Environment");
    }
}

impl Environment {
    pub fn new(parent: Option<&Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            map: HashMap::new(),
            parent: match parent {
                Some(p) => Some(Rc::clone(p)),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: &str, val: Object) {
        self.map.insert(key.to_string(), val);
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        if let Some(val) = self.map.get(key) {
            Some(val.clone())
        } else {
            self.parent.as_ref()?.borrow().get(key)
        }
    }

    pub fn _repr(&self) -> String {
        let keys_repr = self
            .map
            .keys()
            .fold("".to_string(), |acc, x| match acc.chars().count() {
                0 => x.clone(),
                _ => format!("{},{}", acc, x),
            });
        let env_repr = format!("Env({})", keys_repr);
        match self.parent.as_ref() {
            Some(p) => format!("{} -> {}", p.borrow()._repr(), env_repr),
            None => env_repr,
        }
    }
}
