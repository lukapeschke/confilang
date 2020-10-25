use crate::color;
use crate::token::Token;
use crate::utils::HashableFloat;

#[derive(Debug)]
pub struct Lexer<'a> {
    raw: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    len: usize,
    read_pos: usize,
    token_start: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    fn read_char(&mut self) -> Option<char> {
        if self.read_pos >= self.len {
            return None;
        }
        let ch = self.chars.next()?;
        self.ch = ch;
        self.read_pos += 1;
        Some(ch)
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn is_ident_ch(c: &char) -> bool {
        ('a' <= *c && *c <= 'z') || ('A' <= *c && *c <= 'Z') || *c == '_'
    }

    fn is_digit(c: &char) -> bool {
        c.is_ascii_digit() || *c == '-' || *c == '.'
    }

    fn is_whitespace(c: &char) -> bool {
        *c == ' ' || *c == '\t' || *c == '\n' || *c == '\r'
    }

    // Multichar token reader
    fn read_token(&mut self, initializer: char, validator: fn(&char) -> bool) -> String {
        let mut output = String::new();
        output.push(initializer);
        loop {
            let opt = self.peek_char();
            // Break if no char was read or if it is not valid
            match opt {
                Some(&c) => {
                    if validator(&c) {
                        output.push(c);
                        // Only moving cursor forward if we handle the char
                        self.read_char();
                    } else {
                        break;
                    }
                }
                None => break,
            }
        }
        output
    }

    fn read_ident(&mut self, initializer: char) -> String {
        self.read_token(initializer, Lexer::is_ident_ch)
    }

    fn read_number(&mut self, initializer: char) -> String {
        self.read_token(initializer, Lexer::is_digit)
    }

    // Tries to parse a string as an int or a float
    fn parse_number(s: &str) -> Token {
        match s.parse::<i32>() {
            Ok(i) => Token::Int(i),
            Err(_) => match s.parse::<f32>() {
                Ok(f) => Token::Float(HashableFloat::new(f)),
                Err(_) => Token::Illegal(s.to_string()),
            },
        }
    }

    fn read_str(&mut self) -> Token {
        let mut output = String::new();
        while let Some(&ch) = self.peek_char() {
            self.read_char();
            match ch {
                '"' => {
                    return Token::Str(output);
                }
                '\\' => {
                    self.read_char();
                    if let Some(escaped_ch) = self.peek_char() {
                        output.push(match escaped_ch {
                            'n' => {
                                self.read_char();
                                '\n'
                            }
                            _ => *escaped_ch,
                        });
                    } else {
                        return Token::Eof;
                    }
                }
                _ => output.push(ch),
            }
        }
        Token::Eof
    }

    fn handle_ident(&mut self, c: char) -> Token {
        if Lexer::is_ident_ch(&c) {
            let ident: String = self.read_ident(c);

            // Add keywords here
            match ident.as_str() {
                "let" => Token::Let,
                "fn" => Token::Fn,
                "true" => Token::True,
                "false" => Token::False,
                "if" => Token::If,
                "else" => Token::Else,
                "return" => Token::Return,
                "and" => Token::And,
                "or" => Token::Or,

                _ => Token::Ident(ident),
            }
        } else if Lexer::is_digit(&c) {
            // Reading a number and trying to parse it
            Lexer::parse_number(&self.read_number(c))
        } else {
            Token::Illegal(c.to_string())
        }
    }

    fn skip_whitespace(&mut self) {
        while {
            // Looping while current char is whitesapce
            match self.peek_char() {
                Some(c) => Lexer::is_whitespace(c),
                None => false,
            }
        } {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.token_start = self.read_pos;
        self.skip_whitespace();
        let c = self.read_char()?;
        Some(match c {
            // operators
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,

            // possible multichar operators
            '=' => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token::Equals
                }
                _ => Token::Assign,
            },
            '!' => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token::Differs
                }
                _ => Token::Bang,
            },
            '<' => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token::Le
                }
                _ => Token::Lt,
            },
            '>' => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token::Ge
                }
                _ => Token::Gt,
            },

            // delimiters
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            ':' => Token::Colon,

            // Strings
            '"' => self.read_str(),

            _ => self.handle_ident(c),
        })
    }

    pub fn cur_token_litteral(&self) -> String {
        self.raw[self.token_start..self.read_pos].to_string()
    }

    fn reset(&mut self) {
        self.chars = self.raw.chars().peekable();
        self.len = self.raw.chars().count();
        self.read_pos = 0;
        self.ch = 0 as char;
    }

    pub fn new(text: &str) -> Option<Lexer> {
        let len = text.chars().count();
        if len < 1 {
            return None;
        }

        Some(Lexer {
            raw: text,
            chars: text.chars().peekable(),
            len,
            read_pos: 0,
            token_start: 0,
            ch: 0 as char,
        })
    }

    pub fn get_all_tokens(&mut self) -> Vec<Token> {
        let mut output = Vec::new();
        while let Some(tok) = self.next_token() {
            output.push(tok.clone())
        }
        self.reset();
        output
    }
}

pub fn with_colors(litteral: &str) -> String {
    if let Some(mut lex) = Lexer::new(litteral) {
        let mut output = Vec::new();
        while let Some(tok) = lex.next_token() {
            let litteral = lex.cur_token_litteral();
            output.push(color::with_colors(&tok, &litteral));
        }
        output.join("")
    } else {
        "".to_string()
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn get_all_tokens(lex: &mut Lexer) -> Vec<Token> {
        let mut output = Vec::new();
        while let Some(tok) = lex.next_token() {
            output.push(tok.clone())
        }
        output
    }

    fn get_all_litterals(lex: &mut Lexer) -> Vec<String> {
        let mut output = Vec::new();
        while let Some(_) = lex.next_token() {
            output.push(lex.cur_token_litteral())
        }
        output
    }

    #[test]
    fn lex_multichar_op() {
        let input = "== != >= <=".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![Token::Equals, Token::Differs, Token::Ge, Token::Le,]
        )
    }

    #[test]
    fn test_get_all_litterals_simple() {
        let input = "== != >= <=".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(get_all_litterals(&mut lex), vec!["==", " !=", " >=", " <="])
    }

    #[test]
    fn test_get_all_litterals_with_whitespace() {
        let input = "==  !=    >=  <=".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_litterals(&mut lex),
            vec!["==", "  !=", "    >=", "  <="]
        )
    }

    #[test]
    fn lex_op() {
        let input = "=+-*/!<>".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![
                Token::Assign,
                Token::Plus,
                Token::Minus,
                Token::Asterisk,
                Token::Slash,
                Token::Bang,
                Token::Lt,
                Token::Gt,
            ]
        );
    }

    #[test]
    fn lex_keywords() {
        let input = "let fn true false if else return and or".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![
                Token::Let,
                Token::Fn,
                Token::True,
                Token::False,
                Token::If,
                Token::Else,
                Token::Return,
                Token::And,
                Token::Or,
            ]
        )
    }

    #[test]
    fn lex_delims() {
        let input = ",;(){}".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![
                Token::Comma,
                Token::Semicolon,
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::RightBrace,
            ]
        );
    }

    #[test]
    fn lex_add() {
        let input = "let f = 10.5;".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![
                Token::Let,
                Token::Ident("f".to_string()),
                Token::Assign,
                Token::Float(HashableFloat::new(10.5)),
                Token::Semicolon,
            ]
        )
    }

    #[test]
    fn lex_str() {
        let input = "let a = \"hello\";".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![
                Token::Let,
                Token::Ident("a".to_string()),
                Token::Assign,
                Token::Str("hello".to_string()),
                Token::Semicolon,
            ]
        )
    }

    #[test]
    fn lex_escaped_str() {
        let input = "\"he\\llo\"".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![Token::Str("hello".to_string())]
        )
    }

    #[test]
    fn lex_escaped_str_carriage_return() {
        let input = "\"he\\\\nllo\"".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![Token::Str("he\nllo".to_string())]
        )
    }

    #[test]
    fn lex_escaped_backslash_str() {
        let input = "\"he\\\\\\llo\"".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![Token::Str("he\\llo".to_string())]
        )
    }

    #[test]
    fn lex_array() {
        let input = "[1, 2, hello]".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![
                Token::LeftBracket,
                Token::Int(1),
                Token::Comma,
                Token::Int(2),
                Token::Comma,
                Token::Ident("hello".to_string()),
                Token::RightBracket,
            ]
        )
    }

    #[test]
    fn lex_float() {
        let input = "
let five = 5;
let ten = 10;
let add = fn(x, y) {  x + y;};
let result = add(five, ten);
"
        .to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![
                // let five = 5;
                Token::Let,
                Token::Ident("five".to_string()),
                Token::Assign,
                Token::Int(5),
                Token::Semicolon,
                // let ten = 10;
                Token::Let,
                Token::Ident("ten".to_string()),
                Token::Assign,
                Token::Int(10),
                Token::Semicolon,
                //let add = fn(x, y) {  x + y;};
                Token::Let,
                Token::Ident("add".to_string()),
                Token::Assign,
                Token::Fn,
                Token::LeftParen,
                Token::Ident("x".to_string()),
                Token::Comma,
                Token::Ident("y".to_string()),
                Token::RightParen,
                Token::LeftBrace,
                Token::Ident("x".to_string()),
                Token::Plus,
                Token::Ident("y".to_string()),
                Token::Semicolon,
                Token::RightBrace,
                Token::Semicolon,
                // let result = add(five, ten);
                Token::Let,
                Token::Ident("result".to_string()),
                Token::Assign,
                Token::Ident("add".to_string()),
                Token::LeftParen,
                Token::Ident("five".to_string()),
                Token::Comma,
                Token::Ident("ten".to_string()),
                Token::RightParen,
                Token::Semicolon,
            ]
        );
    }

    #[test]
    fn lex_hashmap() {
        let input = "{\"one\": \"two\", 1: 2}".to_string();
        let mut lex = Lexer::new(&input).unwrap();
        assert_eq!(
            get_all_tokens(&mut lex),
            vec![
                Token::LeftBrace,
                Token::Str("one".to_string()),
                Token::Colon,
                Token::Str("two".to_string()),
                Token::Comma,
                Token::Int(1),
                Token::Colon,
                Token::Int(2),
                Token::RightBrace,
            ]
        )
    }
}
