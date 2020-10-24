use crate::evaluator;
use crate::lexer;
use crate::parser;

use std::io::{stdin, stdout, Stdout, Write};
use termion::cursor;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};

static PROMPT: &str = "\r~> ";

struct Repl {
    ev: evaluator::Evaluator,
    debug: bool,
    stdout: RawTerminal<Stdout>,
}

#[derive(Debug)]
enum NextLineStatus {
    Continue,
    Stop,
}

impl NextLineStatus {
    pub fn should_stop(&self) -> bool {
        match self {
            NextLineStatus::Continue => false,
            NextLineStatus::Stop => true,
        }
    }
}

impl Repl {
    pub fn new(debug: bool) -> Repl {
        Repl {
            ev: evaluator::Evaluator::new(),
            debug,
            stdout: stdout().into_raw_mode().unwrap(),
        }
    }

    fn print(&mut self, s: &str) -> Result<(), String> {
        if let Err(e) = self.stdout.write(s.as_bytes()) {
            return Err(format!("error while writing to stdout: {}", e));
        }
        if let Err(e) = self.stdout.flush() {
            return Err(format!("error while flushing stdout: {}", e));
        }
        Ok(())
    }

    fn println(&mut self, s: &str) -> Result<(), String> {
        self.print(&format!("{}\r\n", s))
    }

    fn handle_line(&mut self, line: &str) -> Result<(), String> {
        if let Some(mut lex) = lexer::Lexer::new(line) {
            if self.debug {
                self.println(&format!("Tokens are {:#?}", lex.get_all_tokens()))?;
            }
            let mut parser;
            if let Some(p) = parser::Parser::new(&mut lex) {
                parser = p;
            } else {
                return Err("Couldn't create parser".to_string());
            }

            let prog = parser.parse_program()?;
            if self.debug {
                self.println(&format!("Program parses to: {:?}", prog))?;
            }
            let result = self.ev.eval(prog.as_node(), &None)?.repr();
            println!("Program evaluates to: {:?}", result);
            Ok(())
        } else {
            Err("Couldn't create lexer".to_string())
        }
    }

    fn clear(&mut self) -> Result<(), String> {
        self.print(&format!("{}{}", termion::clear::All, cursor::Goto(1, 1)))
    }

    fn write_prompt(&mut self) -> Result<(), String> {
        self.print(PROMPT)
    }

    fn display_line(&mut self, line: &str, newline: bool) -> Result<(), String> {
        let formatted_line = format!("{}{}", PROMPT, line);
        match newline {
            true => self.println(&formatted_line),
            false => self.print(&formatted_line),
        }
    }

    fn next_line(&mut self, buf: &mut String) -> Result<NextLineStatus, String> {
        let stdin = stdin();
        for key_result in stdin.keys() {
            match key_result {
                Ok(k) => match k {
                    Key::Char(c) => match c {
                        '\n' => {
                            self.display_line(&buf, true)?;
                            return Ok(NextLineStatus::Continue);
                        }
                        _ => buf.push(c),
                    },
                    Key::Ctrl('d') | Key::Ctrl('c') | Key::Ctrl('q') => {
                        return Ok(NextLineStatus::Stop)
                    }
                    Key::Ctrl('l') => self.clear()?,
                    _ => (),
                },
                Err(e) => return Err(format!("error while reading stdin: {}", e)),
            }
            self.display_line(&buf, false)?;
        }
        Ok(NextLineStatus::Stop)
    }

    fn loop_(&mut self) -> Result<(), String> {
        let mut buf = String::new();
        let mut stop = NextLineStatus::Continue;
        while !stop.should_stop() {
            if !buf.is_empty() {
                if let Err(e) = self.handle_line(&buf) {
                    self.println(&format!("Error: {}", e))?;
                }
            }
            buf.clear();
            self.write_prompt()?;
            stop = self.next_line(&mut buf)?;
        }
        self.println("bye!")?;
        Ok(())
    }

    pub fn run(&mut self) -> Result<(), String> {
        self.loop_()
    }
}

pub fn run(debug: bool) -> Result<(), String> {
    Repl::new(debug).run()
}
