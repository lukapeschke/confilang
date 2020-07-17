use crate::ast::Representable;
use crate::evaluator;
use crate::lexer;
use crate::parser;

use std::io::Write;

static PROMPT: &str = "~> ";

fn handle_line(line: &str) -> Result<(), String> {
    if let Some(mut lex) = lexer::Lexer::new(line) {
        let mut parser;
        if let Some(p) = parser::Parser::new(&mut lex) {
            parser = p;
        } else {
            return Err("Couldn't create parser".to_string());
        }

        let prog = parser.parse_program()?;
        println!("Program parses to: {:?}", prog.repr());
        println!(
            "Program evaluates to: {:?}",
            evaluator::eval(prog.as_node())?.repr()
        );
        Ok(())
    } else {
        Err("Couldn't create lexer".to_string())
    }
}

pub fn run() -> Result<(), String> {
    let mut buf = String::new();
    loop {
        print!("{}", PROMPT);
        std::io::stdout().flush().unwrap();
        buf.clear();
        match std::io::stdin().read_line(&mut buf) {
            // EOF
            Ok(0) => {
                println!("bye!");
                return Ok(());
            }
            // Normal case
            Ok(_) => {
                if let Err(s) = handle_line(&buf) {
                    eprintln!("Error: {}", s)
                }
            }
            // Oops
            Err(e) => {
                return Err(e.to_string());
            }
        }
    }
}
