#[derive(Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Float(f32),
    Bool(bool),
    Str(String),
}

impl Object {
    pub fn repr(&self) -> String {
        match self {
            Object::Int(i) => format!("{}", i),
            Object::Float(f) => format!("{}", f),
            Object::Str(s) => format!("{}", s),
            Object::Bool(b) => format!("{}", b),
        }
    }
}
