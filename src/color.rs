use crate::token::Token;
use termion::{color, style};

fn ident() -> String {
    format!("{}", color::Fg(color::Yellow))
}

fn reserved() -> String {
    format!("{}{}", color::Fg(color::Blue), style::Bold)
}

fn reset() -> String {
    format!("{}{}", color::Fg(color::Reset), style::Reset)
}

fn bool_() -> String {
    format!("{}", color::Fg(color::Green))
}

fn str_() -> String {
    format!("{}", color::Fg(color::LightMagenta))
}

pub fn with_colors(tok: &Token, litteral: &str) -> String {
    format!(
        "{prefix}{litteral}{reset}",
        litteral = litteral,
        reset = reset(),
        prefix = {
            if tok.is_keyword() {
                reserved()
            } else if tok.is_bool() {
                bool_()
            } else {
                match tok {
                    Token::Ident(_) => ident(),
                    Token::Str(_) => str_(),
                    _ => "".to_string(),
                }
            }
        }
    )
}

pub fn style_prompt(prompt: &str, success: bool) -> String {
    format!(
        "{color}{bold}{prompt}{reset}",
        bold = style::Bold,
        prompt = prompt,
        reset = reset(),
        color = if success {
            color::Fg(color::Green).to_string()
        } else {
            color::Fg(color::Red).to_string()
        }
    )
}
