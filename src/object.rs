#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i32),
    Float(f32),
    Bool(bool),
    Str(String),
    ReturnValue(Box<Object>),
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
        }
    }
}
