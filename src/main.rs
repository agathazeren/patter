//! A simple unoptimized interpreter for lispap (LISt and PAttern Programming)
//! the goal is the simplest possible implementation, for bootstrapping

#![cfg_attr(not(test), allow(dead_code))]

#[macro_use]
mod macros;
mod context;
mod parse;

use lazy_static::lazy_static;
use std::fmt::Debug;

use crate::context::{Bindings, Context};

#[derive(Clone)]
pub enum SExpr {
    Sigil(String),
    List(Vec<SExpr>),
    Ident(Ident),
    Place(Ident),
    Fun(Box<SExpr>, Box<SExpr>),
    UnarySigilApp(String, Box<SExpr>),
    Int(isize),
    Operation(fn(&mut Context) -> SExpr),
    Keyword(Ident),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    NS(Vec<Ident>),
    Name(String),
}

impl SExpr {
    fn eval(&self, mut cxt: &mut Context, debug: bool) -> SExpr {
        use SExpr::*;
        if debug {
            dbg!(&cxt);
            dbg!(self);
        }
        let expr = match self {
            List(ls) => {
                if ls.is_empty() {
                    return List(ls.to_vec());
                }

                match ls[0].clone().eval(cxt, debug) {
                    Fun(fun, args_ptn) => {
                        if let Some(bindings) = args_ptn.match_ptn(&List(
                            ls[1..]
                                .iter()
                                .map(|e| e.eval(&mut cxt, debug))
                                .collect::<Vec<_>>(),
                        )) {
                            cxt.push_scope();
                            cxt.add_bindings(bindings);
                            let expr = fun.eval(&mut cxt, debug);
                            dbg!(&cxt);
                            cxt.pop_scope();
                            expr
                        } else {
                            panic!(
                                "Arguments ({:#?}) did not match for {:?}, requires {:?}",
                                &ls[1..],
                                fun,
                                args_ptn.clone()
                            )
                        }
                    }
                    e => panic!("{:?} is not callable", e),
                }
            }
            UnarySigilApp(sigil, arg) => {
                if let Fun(fun, args_ptn) = Sigil(sigil.clone()).eval(&mut cxt, debug) {
                    if let Some(bindings) = args_ptn.match_ptn(&List(vec![*arg.clone()])) {
                        cxt.push_scope();
                        cxt.add_bindings(bindings);
                        let expr = fun.eval(&mut cxt, debug);
                        cxt.pop_scope();
                        expr
                    } else {
                        panic!(
                                "Argument ({:?}) did not match for unary sigil application {:?}, requires {:?}",
                                arg,
                                fun,
                                args_ptn.clone()
                            )
                    }
                } else {
                    panic!("Cannot use unary application on sigil {:?} that is defined as {:?}, which is not a function.")
                }
            }
            Ident(id) => cxt
                .lookup(&id)
                .unwrap_or_else(|| panic!("Unknown name {:?}", id)),
            Operation(oper) => oper(&mut cxt),
            Sigil(s) => cxt
                .lookup(&make_sigil_ident(s))
                .unwrap_or_else(|| panic!("Use of undefined sigil {}", s)),
            e => e.clone(),
        };
        if debug {
            dbg!(&expr);
        }
        expr
    }

    fn match_ptn(&self, expr: &SExpr) -> Option<Bindings> {
        use SExpr::*;
        match (self, expr) {
            (Keyword(id1), Keyword(id2)) | (Ident(id1), Ident(id2)) if *id1 == *id2 => {
                Some(Bindings::empty())
            }
            (Fun(_, _), Fun(_, _)) => None,
            (List(left), List(right)) => {
                if left.len() != right.len() {
                    return None;
                }
                let mut bindings = Some(Bindings::empty());
                for (left, right) in left.iter().zip(right.iter()) {
                    match left.match_ptn(right) {
                        Some(bind) => bindings = bindings.map(|b| b.join(bind)),
                        None => bindings = None,
                    }
                }
                bindings
            }
            (Int(i1), Int(i2)) if i1 == i2 => Some(Bindings::empty()),
            (Place(id), thing) => Some(Bindings::of(id, thing)),
            _ => None,
        }
    }

    fn quote(self, mut cxt: &mut Context) -> SExpr {
        /*        use SExpr::*;
                match self {
                    List(ls) => List(ls.iter().map(|e| e.eval(&mut cxt)).collect::<Vec<_>>()),
                    e => e,
                }
        */
        use SExpr::*;
        match self {
            List(ls) => List(
                ls.into_iter()
                    .map(|e| e.quote(&mut cxt))
                    .collect::<Vec<_>>(),
            ),
            e => e,
        }
    }

    fn as_int(self) -> Option<isize> {
        if let SExpr::Int(i) = self {
            Some(i)
        } else {
            None
        }
    }

    fn as_ident(self) -> Option<Ident> {
        if let SExpr::Ident(id) = self {
            Some(id)
        } else {
            None
        }
    }

    fn as_list(self) -> Option<Vec<SExpr>> {
        if let SExpr::List(ls) = self {
            Some(ls)
        } else {
            None
        }
    }
}

fn make_sigil_ident(sigil: &str) -> Ident {
    let mut char_names = Vec::new();
    for char in sigil.chars() {
        char_names.push(match char {
            '`' => "tick",
            ',' => "comma",
            '~' => "tilde",
            '!' => "bang",
            '@' => "at",
            '^' => "carrot",
            '&' => "amp",
            '*' => "star",
            '+' => "plus",
            '=' => "eq",
            '|' => "pike",
            '\\' => "backslash",
            ':' => "colon",
            '<' => "left",
            '>' => "right",
            '[' => "bracket", //not really a sigil, but sortof.
            _ => unreachable!(),
        })
    }
    ident!(&format!("#/sigil/{}", char_names.join("-")))
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
            List(v) => {
                write!(f, "List")?;
                f.debug_list().entries(v.iter()).finish()
            }
            UnarySigilApp(sigil, arg) => f
                .debug_tuple("UnarySigilApp")
                .field(sigil)
                .field(arg)
                .finish(),
            Keyword(id) => write!(f, "Keyword({:?})", id),
            Ident(id) => write!(f, "Ident({:?})", id),
            Place(id) => write!(f, "Place({:?})", id),
            Fun(fun, p) => write!(f, "Fun({:?}, {:?})", *fun, *p),
            Int(i) => write!(f, "Int({:?})", i),
            Operation(_) => write!(f, "Operation"),
            Sigil(s) => write!(f, "Sigil({})", s),
        }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Ident::Name(n) => write!(f, "{}", n)?,
            Ident::NS(ns) => {
                match &ns[0] {
                    Ident::Name(n) => write!(f, "{}", n),
                    Ident::NS(ns) => write!(f, "({:?})", ns),
                }?;
                for name in ns.iter().skip(1) {
                    match name {
                        Ident::Name(n) => write!(f, "/{}", n),
                        Ident::NS(ns) => write!(f, "/({:?})", ns),
                    }?;
                }
            }
        };
        Ok(())
    }
}

fn main() {
    /*    use SExpr::*;
        let code = List(vec![
            Ident(ident!("#/do/ret-last")),
            List(vec![
                Sigil("`".to_string()),
                List(vec![LISPAP_STD.clone(), lispap!("(fib 4)")]),
            ]),
        ]);
        dbg!(code.eval(&mut Context::new()));
    */
}

lazy_static! {
    static ref LISPAP_STD_STR: String =
        format!("{}", std::fs::read_to_string("lispap_std/std.lp").unwrap());
}

#[cfg(test)]
mod tests {

    macro_rules! eval_test {
        ($name:ident, $code:expr, $expected:expr) => {
            #[test]
            fn $name() {
                assert_eq!(lispap!($code).eval(&mut Context::new(), true), $expected);
            }
        };
    }

    macro_rules! eval_test_std {
        ($name:ident, $code:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let code = lispap!(&format!("[{} {}]", *LISPAP_STD_STR, $code));
                assert_eq!(
                    *code
                        .eval(&mut Context::new(), true)
                        .as_list()
                        .unwrap()
                        .last()
                        .unwrap(),
                    $expected
                );
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
    eval_test! {multiple_levels_ident, "`foo/bar/baz", Ident(super::Ident::NS(vec![
        super::Ident::Name("foo".to_string()),
        super::Ident::Name("bar".to_string()),
        super::Ident::Name("baz".to_string()),
    ]))}
    eval_test! {quote, "`(1 (#/add 2 3))", List(vec![
        Int(1),
        List(vec![
            Ident(super::Ident::NS(vec![
                super::Ident::Name("#".to_string()),
                super::Ident::Name("add".to_string())
            ])),
            Int(2),
            Int(3),
        ]),
    ])}
    eval_test! {simple_do, "[(#/add 1 2)]", List(vec![Int(3)])}
    eval_test! {eq, "(#/eq 1 1)", Keyword(super::Ident::Name("true".to_string()))}
    eval_test! {neq, "(#/eq 1 2)", Keyword(super::Ident::Name("false".to_string()))}
    eval_test! {eq_lists, "(#/eq `(1 2) `(1 `(2 3)))",
                Keyword(super::Ident::Name("false".to_string()))
    }
    eval_test_std! {uses_std, "std-is-here", Int(42)}
    eval_test_std! {fib_in_std, "(fib 4)", Int(3)}
    eval_test! {list_item_after_sublist, "(#/add (#/add 1 2) 3)", Int(6)}
    eval_test_std! {id_int, "(id 42)", Int(42)}
    eval_test! {sq_brkt, "[,foo]", List(vec![Place(ident!("foo"))])}
    eval_test_std! {def, "(def ,foo 123) foo", Int(123)}
    eval_test_std! {std_works, "3", Int(3)}
    
    #[test]
    fn context() {
        let _ = Context::new();
    }
}
