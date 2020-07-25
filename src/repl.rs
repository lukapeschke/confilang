use crate::evaluator;
use crate::lexer;
use crate::parser;

use std::io::Write;

static PROMPT: &str = "~> ";

struct Repl {
    ev: evaluator::Evaluator,
    debug: bool,
}

impl Repl {
    pub fn new(debug: bool) -> Repl {
        Repl {
            ev: evaluator::Evaluator::new(),
            debug,
        }
    }
    fn handle_line(&mut self, line: &str) -> Result<(), String> {
        if let Some(mut lex) = lexer::Lexer::new(line) {
            if self.debug {
                println!("Tokens are {:#?}", lex.get_all_tokens())
            }
            let mut parser;
            if let Some(p) = parser::Parser::new(&mut lex) {
                parser = p;
            } else {
                return Err("Couldn't create parser".to_string());
            }

            let prog = parser.parse_program()?;
            if self.debug {
                println!("Program parses to: {:?}", prog);
            }
            let result = self.ev.eval(prog.as_node(), &None)?.repr();
            println!("Program evaluates to: {:?}", result);
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

pub fn run(debug: bool) -> Result<(), String> {
    Repl::new(debug).run()
}
