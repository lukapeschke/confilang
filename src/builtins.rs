use crate::object::{BuiltIn, Object};

fn len(args: &[Object]) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("len() takes exactly one argument".to_string());
    }
    match &args[0] {
        Object::Str(s) => Ok(Object::Int(s.chars().count() as i32)),
        _ => Err(format!("unsupported type for len: {:?}", args[0])),
    }
}

pub fn get(name: &str) -> Option<BuiltIn> {
    match name {
        "len" => Some(BuiltIn::new("len".to_string(), len)),
        _ => None,
    }
}
