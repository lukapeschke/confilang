use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub struct Environment {
    map: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            map: HashMap::new(),
        }
    }

    pub fn set(&mut self, key: &str, val: Object) {
        self.map.insert(key.to_string(), val);
    }
    pub fn get(&mut self, key: &str) -> Option<Object> {
        Some(self.map.get(key)?.clone())
    }
}
