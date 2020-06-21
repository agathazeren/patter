//! each parse_at function should start with the offset pointing to the first character of the thing to be parsed, and should end pointing to the character after the end of the thing to be parsed.

use super::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Open(Grouping),
    Close(Grouping),
    Whitespaces,
    Word(String),
    NSOperator,
    Sigil(String),
    Num(isize),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Grouping {
    Paren,
    Bracket,
}

pub fn lex(source: &str) -> Vec<Token> {
    use Token::*;
    let mut tokens = Vec::new();
    let mut offset: &mut usize = &mut 0;
    while let Some(c) = source[*offset..].chars().nth(0) {
        match c {
            '(' => tokens.push(Open(Grouping::Paren)),
            ')' => tokens.push(Close(Grouping::Paren)),
            '[' => tokens.push(Open(Grouping::Bracket)),
            ']' => tokens.push(Close(Grouping::Bracket)),
            w if w.is_whitespace() => {
                tokens.push(Whitespaces);
                let mut looped = false;
                while source[*offset..].chars().nth(0).unwrap().is_whitespace() {
                    inc_char_idx(source, &mut offset);
                    looped = true;
                }
                if looped {
                    dec_char_idx(source, &mut offset)
                }
            }
            d if d == '-' || d.is_ascii_digit() => {
                tokens.push(Num(parse_int_at(source, &mut offset)))
            }
            c if is_ident_char(c) => {
                tokens.push(Word(eat_while(is_ident_char, source, &mut offset)));
            }
            '/' => tokens.push(NSOperator),
            s if is_sigil_char(s) => {
                tokens.push(Sigil(eat_while(is_sigil_char, source, &mut offset)));
            }
            c => panic!("Unknown character {}", c),
        }
        inc_char_idx(source, &mut offset);
        if *offset > source.len() {
            break;
        }
    }
    tokens
}

fn eat_while<F: Fn(char) -> bool>(pred: F, source: &str, mut offset: &mut usize) -> String {
    let start_offset = *offset;
    while source[*offset..].chars().nth(0).map(&pred) == Some(true) {
        inc_char_idx(source, &mut offset)
    }
    if *offset != start_offset {
        dec_char_idx(source, &mut offset)
    }
    source[start_offset..=*offset].to_string()
}

fn parse_int_at(source: &str, mut offset: &mut usize) -> isize {
    let negate = source[*offset..].chars().nth(0).unwrap() == '-';
    if negate {
        inc_char_idx(source, &mut offset);
    }
    let mut n_str = String::new();
    loop {
        let d = match source[*offset..].chars().nth(0) {
            Some(d) => d,
            None => break,
        };
        if !d.is_ascii_digit() {
            dec_char_idx(source, &mut offset);
            break;
        }
        n_str.push(d);
        inc_char_idx(source, &mut offset);
    }
    n_str
        .parse::<usize>()
        .map(|n| if negate { -(n as isize) } else { n as isize })
        .expect("Could not parse a number")
}

fn is_sigil_char(c: char) -> bool {
    [
        '~', '\'', ',', '`', '!', '@', '^', '&', '*', '+', '=', '|', '\\', ':', '>', '<',
    ]
    .contains(&c)
}

pub fn parse(source: &[Token]) -> SExpr {
    let mut idx = 0;
    parse_at(source, &mut idx)
}

fn parse_at(source: &[Token], offset: &mut usize) -> SExpr {
    use Token::*;
    match &source[*offset] {
        Whitespaces => {
            *offset += 1;
            parse_at(source, offset)
        }
        Open(Grouping::Paren) => parse_list_at(source, offset, Grouping::Paren),
        Open(Grouping::Bracket) => SExpr::UnarySigilApp(
            "[".to_string(),
            Box::new(parse_list_at(source, offset, Grouping::Bracket)),
        ),
        Sigil(s) => {
            *offset += 1;
            if source[*offset] != Whitespaces {
                SExpr::UnarySigilApp(s.to_string(), Box::new(parse_at(source, offset)))
            } else {
                SExpr::Sigil(s.to_string())
            }
        }
        Num(i) => {
            *offset += 1;
            SExpr::Int(*i)
        }
        Word(_) => SExpr::Ident(parse_ident_at(source, offset)),
        t => panic!("Unknown token {:?}", t),
    }
}

fn parse_list_at(source: &[Token], offset: &mut usize, grouping: Grouping) -> SExpr {
    use Token::*;
    assert_eq!(source[*offset], Open(grouping));
    *offset += 1;
    let mut list = Vec::new();
    while source[*offset] != Close(grouping) {
        list.push(parse_at(source, offset));
        while *offset < source.len() && source[*offset] == Whitespaces {
            *offset += 1;
        }
        if *offset >= source.len() {
            panic!("Unclosed list")
        }
    }
    *offset += 1;
    assert_eq!(source[*offset - 1], Close(grouping));
    SExpr::List(list)
}

pub fn parse_ident(source: &[Token]) -> super::Ident {
    parse_ident_at(source, &mut 0)
}

fn parse_ident_at(source: &[Token], offset: &mut usize) -> super::Ident {
    use Token::*;
    let mut names = Vec::new();
    while let Word(w) = &source[*offset] {
        if *offset >= source.len() {
            break;
        }
        names.push(w);
        *offset += 1;
        if *offset >= source.len() {
            break;
        }
        match source[*offset] {
            NSOperator => *offset += 1,
            Whitespaces | Close(_) => {}
            ref t => panic!(
                "Illegal Identifier. Got {:?} after {:?}",
                t.clone(),
                w.clone()
            ),
        }
    }
    if names.len() == 1 {
        super::Ident::Name(names[0].clone())
    } else {
        super::Ident::NS(
            names
                .into_iter()
                .map(|n| super::Ident::Name(n.clone()))
                .collect::<Vec<_>>(),
        )
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || ['\'', '#', '-', '_', '?'].contains(&c)
}

fn inc_char_idx(source: &str, idx: &mut usize) {
    *idx += 1;
    while !source.is_char_boundary(*idx) && *idx < source.len() {
        *idx += 1;
    }
}

fn dec_char_idx(source: &str, idx: &mut usize) {
    *idx -= 1;
    while !source.is_char_boundary(*idx) {
        *idx -= 1;
    }
}
