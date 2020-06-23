mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use termion;

fn main() {
    let stdin = std::io::stdin();
    if termion::is_tty(&stdin) {
        repl::run().unwrap();
    } else {
        eprintln!("I only speak TTY for now :-(")
    }
}
