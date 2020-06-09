use super::SExpr::*;
use super::*;

pub fn parse(source: &str) -> SExpr {
    parse_at(source, &mut 0)
}

fn parse_at(source: &str, mut offset: &mut usize) -> SExpr {
    let c = source[*offset..].chars().nth(0)?;
    inc_char_idx(source, &mut offset);
    let expr = match c {
        ' ' | '\n' => parse_at(source, offset),
        '(' => parse_list_at(source, &mut offset),
        ':' => Keyword(parse_ident_at(source, &mut offset)?),
        ',' => Place(parse_ident_at(source, &mut offset)?),
        '`' => List(vec![Quote, parse_at(source, &mut offset)]),
        c if (c.is_ascii_digit() || c == '-') => {
            dec_char_idx(source, &mut offset);
            parse_int_at(source, offset)
        }
        c if is_ident_char(c) => {
            dec_char_idx(source, &mut offset);
            Ident(parse_ident_at(source, &mut offset)?)
        }
        c => CompileError(format!("Unknown character {}", c)),
    };
    expr
}

fn parse_list_at(source: &str, mut offset: &mut usize) -> SExpr {
    let mut list = Vec::new();
    loop {
        if source[*offset..].chars().skip_while(|c| c.is_whitespace()).nth(0)? == ')' {
            break;
        }
        list.push(parse_at(source, &mut offset));
        inc_char_idx(source, &mut offset);
    }
    List(list)
}

pub fn parse_ident(source: &str) -> Result<super::Ident, SExprError> {
    parse_ident_at(source, &mut 0)
}

fn parse_ident_at(source: &str, mut offset: &mut usize) -> Result<super::Ident, SExprError> {
    let mut names = Vec::new();
    let mut name = String::new();
    loop {
        let c = match source[*offset..].chars().nth(0) {
            Some(c) => c,
            None => break,
        };
        if c.is_whitespace() {
            break;
        }
        match c {
            c if is_ident_char(c) => {
                name.push(c);
            }
            '(' => names.push(parse_ident_at(source, &mut offset)?),
            ')' => {
                dec_char_idx(source, &mut offset);
                break;
            }
            '/' => {
                names.push(super::Ident::Name(name));
                name = String::new();
            }
            c => {
                return Err(format!("Bad char {} in identifier", c).into());
            }
        }
        inc_char_idx(source, &mut offset);
    }
    Ok(if names.len() == 0 {
        super::Ident::Name(name)
    } else {
        names.push(super::Ident::Name(name));
        super::Ident::NS(names)
    })
}

fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || ['\'', '#', '-', '_'].contains(&c)
}

fn parse_int_at<'a>(source: &'a str, mut offset: &mut usize) -> SExpr {
    let negate = source[*offset..].chars().nth(0)? == '-';
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
        .map(|n| Int(n))
        .unwrap_or(CompileError(format!("Could not parse number {}", n_str)))
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
