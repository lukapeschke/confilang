use crate::object::{BuiltIn, Object};

fn ensure_args_len(args: &[Object], expected_len: usize, fn_name: &str) -> Result<(), String> {
    if args.len() == expected_len {
        Ok(())
    } else {
        Err(format!(
            "{}() takes {} argument(s), got {}",
            fn_name,
            expected_len,
            args.len()
        ))
    }
}

fn len(args: &[Object]) -> Result<Object, String> {
    ensure_args_len(args, 1, "len")?;
    match &args[0] {
        Object::Str(s) => Ok(Object::Int(s.chars().count() as i32)),
        Object::Array(a) => Ok(Object::Int(a.len() as i32)),
        _ => Err(format!("unsupported type for len: {:?}", args[0])),
    }
}

fn head(args: &[Object]) -> Result<Object, String> {
    ensure_args_len(args, 1, "head")?;
    match &args[0] {
        Object::Array(a) => Ok(a.head()),
        _ => Err(format!("unsupported type for head: {:?}", args[0])),
    }
}

fn tail(args: &[Object]) -> Result<Object, String> {
    ensure_args_len(args, 1, "tail")?;
    match &args[0] {
        Object::Array(a) => Ok(a.tail()),
        _ => Err(format!("unsupported type for tail: {:?}", args[0])),
    }
}

pub fn get(name: &str) -> Option<BuiltIn> {
    match name {
        "len" => Some(BuiltIn::new("len".to_string(), len)),
        "head" => Some(BuiltIn::new("head".to_string(), head)),
        "tail" => Some(BuiltIn::new("tail".to_string(), tail)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::evaluator::tests::test_multiple_eval;
    use crate::object::Array;

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
        test_multiple_eval(vec![
            ("let a = [1, 2, 3, 4]; head(a)".to_string(), Object::Int(1)),
            ("let a = []; head(a)".to_string(), Object::None),
        ]);
    }

    #[test]
    fn test_eval_tail_builtin() {
        test_multiple_eval(vec![
            (
                "let a = [1, 2, 3, 4]; tail(a)".to_string(),
                Object::Array(Array::new(&[
                    Object::Int(2),
                    Object::Int(3),
                    Object::Int(4),
                ])),
            ),
            (
                "let a = [0, 1]; tail(a)".to_string(),
                Object::Array(Array::new(&[Object::Int(1)])),
            ),
            (
                "let a = [0]; tail(a)".to_string(),
                Object::Array(Array::new(&[])),
            ),
        ]);
    }
}
