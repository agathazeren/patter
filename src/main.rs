//! A simple unoptimized interpreter for lispap (LISt and PAttern Programming)
//! the goal is the simplest possible implementation, for bootstrapping

#![feature(try_trait)]
#![cfg_attr(not(test), allow(dead_code))]

use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::Extend;
use std::ops::Try;
use std::option::NoneError;

#[derive(Clone)]
pub enum SExpr {
    List(Vec<SExpr>),
    Keyword(Ident),
    Ident(Ident),
    Place(Ident),
    Quote,
    CompileError(String),
    Fun(Box<SExpr>, Box<SExpr>),
    Int(isize),
    Operation(fn(&mut Context) -> SExpr),
}

pub struct Context {
    bindings: Bindings,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ident {
    NS(Vec<Ident>),
    Name(String),
}

#[derive(Clone, Debug)]
struct Bindings(HashMap<Ident, SExpr>);

impl SExpr {
    fn eval(self, mut cxt: &mut Context) -> SExpr {
        use SExpr::*;
        match self {
            List(ls) => {
                if ls.is_empty() {
                    return List(ls);
                }
                match ls[0].clone().eval(cxt) {
                    Fun(fun, args_ptn) => {
                        if let Some(bindings) = args_ptn.match_ptn(&List(ls[1..].to_vec())) {
                            fun.eval(&mut Context {
                                bindings: cxt.bindings.clone().join(bindings),
                                ..*cxt
                            })
                        } else {
                            CompileError(format!(
                                "Arguments ({:?}) did not match for {:?}, requires {:?}",
                                &ls[1..],
                                fun,
                                args_ptn.clone()
                            ))
                        }
                    }
                    Quote => {
                        if ls.len() != 2 {
                            CompileError("Quote only takes a single argument".to_string())
                        } else {
                            ls[1].clone()
                        }
                    }
                    e => CompileError(format!("{:?} is not callable", e)),
                }
            }
            Ident(id) => cxt
                .lookup(&id)
                .unwrap_or(CompileError(format!("Unknown name {:?}", id))),
            Operation(oper) => oper(&mut cxt),
            e => e,
        }
    }

    fn match_ptn(&self, ptn: &SExpr) -> Option<Bindings> {
        use SExpr::*;
        match (self, ptn) {
            (CompileError(_), _) | (_, CompileError(_)) => None,
            (Keyword(id1), Keyword(id2)) | (Ident(id1), Ident(id2)) if *id1 == *id2 => {
                Some(Bindings::new())
            }
            (Quote, Quote) => Some(Bindings::new()),
            (Fun(_, _), Fun(_, _)) => None,
            (List(left), List(right)) => {
                let mut bindings = Some(Bindings::new());
                for (left, right) in left.iter().zip(right.iter()) {
                    match left.match_ptn(right) {
                        Some(bind) => bindings = bindings.map(|b| b.join(bind)),
                        None => bindings = None,
                    }
                }
                bindings
            }
            (Place(id), thing) => Some(Bindings::of(id, thing)),
            _ => None,
        }
    }
}

impl Bindings {
    fn join(mut self, other: Bindings) -> Bindings {
        self.0.extend(other.0.into_iter());
        self
    }

    fn new() -> Bindings {
        Bindings(HashMap::new()).join(Bindings::of(
            &parse::parse_ident("#/add").unwrap(),
            &SExpr::Fun(
                Box::new(SExpr::Operation(|cxt| {
                    match dbg!((
                        cxt.lookup(&parse::parse_ident("lhs").unwrap())
                            .map(|e| e.eval(cxt)),
                        cxt.lookup(&parse::parse_ident("rhs").unwrap())
                            .map(|e| e.eval(cxt)),
                    )) {
                        (Some(SExpr::Int(lhs)), Some(SExpr::Int(rhs))) => SExpr::Int(lhs + rhs),
                        _ => unreachable!(),
                    }
                })),
                Box::new(dbg!(parse::parse("(,lhs ,rhs)"))),
            ),
        ))
    }

    fn of(ident: &Ident, value: &SExpr) -> Bindings {
        Bindings({
            let mut m = HashMap::new();
            m.insert(ident.clone(), value.clone());
            m
        })
    }
}

impl Context {
    fn lookup(&self, ident: &Ident) -> Option<SExpr> {
        dbg!(self.bindings.0.get(&ident).cloned())
    }

    fn new() -> Context {
        Context {
            bindings: Bindings::new(),
        }
    }
}

impl PartialEq for SExpr {
    fn eq(&self, other: &SExpr) -> bool {
        use SExpr::*;
        match (self, other) {
            (List(v0), List(v1)) if v0 == v1 => true,
            (Keyword(id0), Keyword(id1)) | (Ident(id0), Ident(id1)) | (Place(id0), Place(id1))
                if id0 == id1 =>
            {
                true
            }
            (Quote, Quote) => true,
            (CompileError(e0), CompileError(e1)) if e0 == e1 => true,
            (Fun(f0, p0), Fun(f1, p1)) if f0 == f1 && p0 == p1 => true,
            (Int(i0), Int(i1)) if i0 == i1 => true,
            _ => false,
        }
    }
}

impl Debug for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use SExpr::*;
        match self {
            List(v) => write!(f, "List({:?})", v),
            Keyword(id) => write!(f, "Keyword({:?})", id),
            Ident(id) => write!(f, "Ident({:?})", id),
            Place(id) => write!(f, "Place({:?})", id),
            Quote => write!(f, "Quote"),
            CompileError(e) => write!(f, "CompileError({:?})", e),
            Fun(fun, p) => write!(f, "Fun({:?}, {:?})", *fun, *p),
            Int(i) => write!(f, "Int({:?})", i),
            Operation(_) => write!(f, "Operation"),
        }
    }
}

#[derive(Debug)]
pub struct SExprError(String);

impl Try for SExpr {
    type Ok = SExpr;
    type Error = SExprError;

    fn into_result(self) -> Result<SExpr, SExprError> {
        Ok(self)
    }

    fn from_ok(expr: SExpr) -> SExpr {
        expr
    }

    fn from_error(err: SExprError) -> SExpr {
        SExpr::CompileError(err.0)
    }
}

impl From<String> for SExprError {
    fn from(s: String) -> SExprError {
        SExprError(s)
    }
}

impl From<NoneError> for SExprError {
    fn from(_: NoneError) -> SExprError {
        SExprError("Program unexpectly ended".to_string())
    }
}

mod parse {
    use super::SExpr::*;
    use super::*;

    pub fn parse(source: &str) -> SExpr {
        parse_at(source, &mut 0)
    }

    fn parse_at(source: &str, mut offset: &mut usize) -> SExpr {
        dbg!("parse_at");
        let c = source[*offset..].chars().nth(0)?;
        dbg!(c);
        inc_char_idx(source, &mut offset);
        let expr = match c {
            ' ' | '\n' => parse_at(source, offset),
            '(' => parse_list_at(source, &mut offset),
            ':' => Keyword(parse_ident_at(source, &mut offset)?),
            ',' => dbg!(Place(parse_ident_at(source, &mut offset)?)),
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
        dbg!("parse_list_at");
        let mut list = Vec::new();
        loop {
            dbg!();
            if source[*offset..].chars().nth(0)? == ')' {
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
        dbg!("parse_ident_at");
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
            dbg!(c);
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
        dbg!(Ok(if names.len() == 0 {
            super::Ident::Name(name)
        } else {
            names.push(super::Ident::Name(name));
            super::Ident::NS(names)
        }))
    }

    fn is_ident_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || ['\'', '#', '-', '_'].contains(&c)
    }

    fn parse_int_at<'a>(source: &'a str, mut offset: &mut usize) -> SExpr {
        let negate = source[*offset..].chars().nth(0)? == '-';
        if negate {
            inc_char_idx(source, &mut offset);
        }
        dbg!("parse_int_at");
        let mut n_str = String::new();
        loop {
            let d = match source[*offset..].chars().nth(0) {
                Some(d) => d,
                None => break,
            };
            dbg!(d);
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

fn main() {
    dbg!(parse::parse("(,lhs ,rhs)"));
}

#[cfg(test)]
mod tests {

    macro_rules! eval_test {
        ($name:ident, $code:expr, $expected:expr) => {
            #[test]
            fn $name() {
                assert_eq!(parse::parse($code).eval(&mut Context::new()), $expected);
            }
        };
    }

    use super::SExpr::*;
    use super::*;

    eval_test! {empty_list, "()", List(vec![])}
    eval_test! {lone_number, "5", Int(5)}
    eval_test! {neg_number, "-5", Int(-5)}
    eval_test! {one_plus_one, "(#/add 1 1)", Int(2)}
    eval_test! {one_plue_one_plus_one, "(#/add 1 (#/add 1 1))", Int(3)}
    eval_test! {quote, "`(1 (#/add 2 3))", List(vec![Int(1), List(vec![
        SExpr::Ident(super::Ident::NS(vec![
            super::Ident::Name("#".to_string()),
            super::Ident::Name("add".to_string())
        ])),
        Int(2), Int(3)
    ])])}
}
