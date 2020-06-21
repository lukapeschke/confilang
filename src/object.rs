#[derive(Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Float(f32),
    Bool(bool),
    Str(String),
}
