mod ast;
mod lexer;
mod repl;
mod token;

fn main() {
    repl::run().unwrap();
}
