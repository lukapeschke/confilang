mod ast;
mod builtins;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;
mod utils;

use clap::Clap;

/// A basic example
#[derive(Clap, Debug)]
#[clap(name = "confilang")]
struct Opt {
    #[clap(short, long)]
    debug: bool,
}

fn main() {
    let opt = Opt::parse();
    let stdin = std::io::stdin();
    if termion::is_tty(&stdin) {
        repl::run(opt.debug).unwrap();
    } else {
        eprintln!("I only speak TTY for now :-(")
    }
}
