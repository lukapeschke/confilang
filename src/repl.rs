use crate::lexer;

use std::io::Write;

static PROMPT: &str = "~> ";

fn handle_line(line: &String) -> Result<i32, i32> {
    match lexer::Lexer::new(line) {
        Some(mut lex) => {
            println!("{:?}", lex.get_all_tokens());
            Ok(0)
        }
        _ => Err(1),
    }
}

pub fn run() -> Result<i32, i32> {
    let mut buf = String::new();
    loop {
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        buf.clear();
        match std::io::stdin().read_line(&mut buf) {
            // EOF
            Ok(0) => {
                println!("bye!");
                return Ok(0);
            }
            // Normal case
            Ok(_) => {
                handle_line(&buf)?;
            }
            // Oops
            Err(_) => {
                return Err(1);
            }
        }
    }
}
