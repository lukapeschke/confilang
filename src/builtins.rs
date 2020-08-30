use crate::object::{BuiltIn, Object};

fn len(args: &[Object]) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("len() takes exactly one argument".to_string());
    }
    match &args[0] {
        Object::Str(s) => Ok(Object::Int(s.chars().count() as i32)),
        Object::Array(a) => Ok(Object::Int(a.len() as i32)),
        _ => Err(format!("unsupported type for len: {:?}", args[0])),
    }
}

fn head(args: &[Object]) -> Result<Object, String> {
    if args.len() != 1 {
        return Err("head() takes exactly one argument".to_string());
    }
    match &args[0] {
        Object::Array(a) => Ok(a.head()),
        _ => Err(format!("unsupported type for head: {:?}", args[0])),
    }
}

pub fn get(name: &str) -> Option<BuiltIn> {
    match name {
        "len" => Some(BuiltIn::new("len".to_string(), len)),
        "head" => Some(BuiltIn::new("head".to_string(), head)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evaluator::tests::test_multiple_eval;

    #[test]
    fn test_eval_len_builtin() {
        test_multiple_eval(vec![
            ("let a = len(\"hello\"); a".to_string(), Object::Int(5)),
            ("len(\"\")".to_string(), Object::Int(0)),
            ("len([1, 2, 3, 4])".to_string(), Object::Int(4)),
        ])
    }

    #[test]
    fn test_eval_head_builtin() {
        test_multiple_eval(vec![(
            "let a = [1, 2, 3, 4]; head(a)".to_string(),
            Object::Int(1),
        )]);
    }
}
