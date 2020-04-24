use crate::token::Token;

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    len: usize,
    read_pos: usize,
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
        c.is_ascii_digit()
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

    fn handle_ident(&mut self, c: char) -> Token {
        if Lexer::is_ident_ch(&c) {
            let ident: String = self.read_ident(c);

            // Add keywords here
            match ident.as_str() {
                "let" => Token::Let,
                "fn" => Token::Fn,
                _ => Token::Ident(ident),
            }
        } else if Lexer::is_digit(&c) {
            // Reading a number and trying to parse it
            let ident: String = self.read_number(c);
            match ident.parse::<i32>() {
                Ok(i) => Token::Int(i),
                Err(_) => Token::Illegal(ident),
            }
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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.read_char() {
            Some(c) => match c {
                '=' => Token::Assign,
                '+' => Token::Plus,
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '(' => Token::LeftParen,
                ')' => Token::RightParen,
                '{' => Token::LeftBrace,
                '}' => Token::RightBrace,
                _ => self.handle_ident(c),
            },
            None => Token::Eof,
        }
    }

    pub fn new(text: &'a String) -> Option<Lexer<'a>> {
        let len = text.chars().count();
        if len < 1 {
            return None;
        }

        Some(Lexer {
            chars: text.chars().peekable(),
            len: len,
            read_pos: 0,
            ch: 0 as char,
        })
    }
}

#[cfg(test)]
mod tests {

    use super::Lexer;
    use super::Token;

    fn get_all_tokens(lex: &mut Lexer) -> Vec<Token> {
        let mut output = Vec::new();
        loop {
            let tok = lex.next_token();
            output.push(tok.clone());
            match &tok {
                Token::Eof => break,
                _ => (),
            }
        }
        output
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
                Token::Eof,
            ]
        );
    }

    #[test]
    fn lex_add() {
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
                Token::Eof,
            ]
        );
    }
}
