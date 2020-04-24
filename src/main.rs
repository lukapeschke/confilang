mod lexer;
mod token;

use lexer::Lexer;

fn main() {
    let text = "Coucou".to_string();
    let mut lex = Lexer::new(&text).unwrap();
    let next_token = lex.next_token();
    println!("Hello, {:#?} {:?}!", &lex, next_token);
}
