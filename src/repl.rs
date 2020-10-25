use crate::evaluator;
use crate::lexer;
use crate::parser;

use std::io::{stdin, stdout, Stdout, Write};
use termion::cursor;
use termion::cursor::{DetectCursorPos, Goto};
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};

static PROMPT: &str = "\r~> ";
static PROMPT_LEN: usize = 3;

struct Repl {
    ev: evaluator::Evaluator,
    debug: bool,
    stdout: RawTerminal<Stdout>,
    buf_ptr: usize,
}

#[derive(Debug)]
enum NextLineStatus {
    Continue,
    Stop,
    Previous,
    Next,
}

impl NextLineStatus {
    pub fn should_stop(&self) -> bool {
        match self {
            NextLineStatus::Stop => true,
            _ => false,
        }
    }
}

impl Repl {
    pub fn new(debug: bool) -> Repl {
        Repl {
            ev: evaluator::Evaluator::new(),
            debug,
            stdout: stdout().into_raw_mode().unwrap(),
            buf_ptr: 0,
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

    fn get_cursor_pos(&mut self, buf_ptr: usize) -> Result<Goto, String> {
        let (_, cur_pos_y) = match self.stdout.cursor_pos() {
            Ok(pos) => pos,
            Err(e) => return Err(format!("error while retrieving cursor position: {}", e)),
        };
        Ok(Goto((PROMPT_LEN + buf_ptr + 1) as u16, cur_pos_y))
    }

    fn display_line(&mut self, line: &str, buf_ptr: usize, newline: bool) -> Result<(), String> {
        let cursor_pos = self.get_cursor_pos(buf_ptr)?;
        let clear = termion::clear::AfterCursor;
        let formatted_line = format!(
            "{prompt}{line}{cursor_pos}{clear}",
            prompt = PROMPT,
            line = line,
            cursor_pos = cursor_pos,
            clear = clear
        );
        match newline {
            true => self.println(&formatted_line),
            false => self.print(&formatted_line),
        }
    }

    fn reset_buf_ptr(&mut self, buf: &mut String, reset: bool) {
        self.buf_ptr = if reset { 0 } else { buf.chars().count() };
    }

    fn next_line(&mut self, buf: &mut String, reset: bool) -> Result<NextLineStatus, String> {
        let stdin = stdin();
        self.reset_buf_ptr(buf, reset);

        let left_offset = |i| if i > 0 { 1 } else { 0 };
        let right_offset = |i, max_len| if i < max_len { 1 } else { 0 };

        self.display_line(&buf, self.buf_ptr, false)?;
        for key_result in stdin.keys() {
            match key_result {
                Ok(k) => match k {
                    Key::Char(c) => match c {
                        '\n' => {
                            self.display_line(&buf, buf.chars().count(), true)?;
                            return Ok(NextLineStatus::Continue);
                        }
                        _ => {
                            if self.buf_ptr == buf.len() {
                                buf.push(c);
                            } else {
                                buf.insert(self.buf_ptr, c);
                            }
                            self.buf_ptr += 1;
                        }
                    },
                    Key::Ctrl('d') | Key::Ctrl('c') | Key::Ctrl('q') => {
                        return Ok(NextLineStatus::Stop)
                    }
                    Key::Ctrl('l') => self.clear()?,
                    Key::Left => self.buf_ptr -= left_offset(self.buf_ptr),
                    Key::Right => self.buf_ptr += right_offset(self.buf_ptr, buf.chars().count()),
                    Key::Home => self.buf_ptr = 0,
                    Key::End => self.buf_ptr = buf.chars().count(),
                    Key::Up => return Ok(NextLineStatus::Previous),
                    Key::Down => return Ok(NextLineStatus::Next),
                    _ => (),
                },
                Err(e) => return Err(format!("error while reading stdin: {}", e)),
            }
            self.display_line(&buf, self.buf_ptr, false)?;
        }
        Ok(NextLineStatus::Stop)
    }

    fn loop_(&mut self) -> Result<(), String> {
        let mut buf = String::new();
        let mut status = NextLineStatus::Continue;
        let mut history = vec!["".to_string()];
        let mut hist_ptr = 0;
        let mut reset = true;
        while !status.should_stop() {
            self.write_prompt()?;
            status = self.next_line(&mut buf, reset)?;
            reset = match status {
                NextLineStatus::Continue => {
                    // self.println(&format!("CONTINUE BUF |{}|", buf));
                    if !buf.is_empty() && !history.contains(&buf) {
                        history.push(buf.clone());
                    }
                    if !buf.is_empty() {
                        if let Err(e) = self.handle_line(&buf) {
                            self.println(&format!("Error: {}", e))?;
                        }
                    }
                    hist_ptr = history.len();
                    buf.clear();
                    true
                }
                NextLineStatus::Previous => {
                    if hist_ptr > 0 {
                        hist_ptr -= 1;
                    }
                    buf = history[hist_ptr].clone();
                    false
                }
                NextLineStatus::Next => {
                    hist_ptr += 1;
                    if hist_ptr >= history.len() {
                        hist_ptr = history.len();
                        buf.clear();
                    } else {
                        buf = history[hist_ptr].clone();
                    }
                    false
                }
                _ => true,
            };
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
