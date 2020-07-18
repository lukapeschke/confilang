use crate::ast::Representable;
use crate::evaluator;
use crate::lexer;
use crate::parser;

use std::io::Write;

static PROMPT: &str = "~> ";

struct Repl {
    ev: evaluator::Evaluator,
}

impl Repl {
    pub fn new() -> Repl {
        Repl {
            ev: evaluator::Evaluator::new(),
        }
    }
    fn handle_line(&mut self, line: &str) -> Result<(), String> {
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
                self.ev.eval(prog.as_node())?.repr()
            );
            Ok(())
        } else {
            Err("Couldn't create lexer".to_string())
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
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
                    if let Err(s) = self.handle_line(&buf) {
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
}

pub fn run() -> Result<(), String> {
    Repl::new().run()
}
