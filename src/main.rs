//! A simple unoptimized interpreter for lispap (LISt and PAttern Programming)
//! the goal is the simplest possible implementation, for bootstrapping

#![feature(try_trait)]
#![cfg_attr(not(test), allow(dead_code))]

mod parse;

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
    Unquote,
    CompileError(String),
    Fun(Box<SExpr>, Box<SExpr>),
    Int(isize),
    Operation(fn(&mut Context) -> SExpr),
}

#[derive(Clone, Debug)]
pub struct Context {
    bindings: Bindings,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    NS(Vec<Ident>),
    Name(String),
}

#[derive(Clone, Debug)]
struct Bindings(HashMap<Ident, SExpr>);

impl SExpr {
    fn eval(&self, mut cxt: &mut Context) -> SExpr {
        dbg!(self);
        dbg!(cxt.clone());
        use SExpr::*;
        match self {
            List(ls) => {
                if ls.is_empty() {
                    return List(ls.to_vec());
                }
                match ls[0].clone().eval(cxt) {
                    Fun(fun, args_ptn) => {
                        if let Some(bindings) = args_ptn.match_ptn(&List(
                            ls[1..].iter()
                                .map(|e| dbg!(dbg!(e).eval(&mut cxt)))
                                .collect::<Vec<_>>(),
                        )) {
                            *cxt = Context {
                                bindings: cxt.bindings.clone().join(bindings),
                                ..*cxt
                            };
                            fun.eval(&mut cxt)
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
                            ls[1].clone().quote(&mut cxt)
                        }
                    }
                    e => CompileError(format!("{:?} is not callable", e)),
                }
            }
            Ident(id) => cxt
                .lookup(&id)
                .unwrap_or(CompileError(format!("Unknown name {:?}", id))),
            Operation(oper) => oper(&mut cxt),
            e => e.clone(),
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

    fn quote(self, mut cxt: &mut Context) -> SExpr {
        use SExpr::*;
        match self {
            List(ls) => {
                if ls.get(0) == Some(&Unquote) {
                    ls[1].eval(&mut cxt)
                } else {
                    List(
                        ls.into_iter()
                            .map(|e| e.quote(&mut cxt))
                            .collect::<Vec<_>>(),
                    )
                }
            }
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
}

macro_rules! get {
    ($ident:expr, $cxt:expr) => {
        $cxt.lookup(&parse::parse_ident($ident).unwrap()).unwrap()
    };
}

macro_rules! primitive {
    ($name:expr, $ptn:expr, $impl:expr, $cxt:ident) => {
        Bindings::of(
            &parse::parse_ident($name).unwrap(),
            &SExpr::Fun(
                #[allow(unused_mut)]
                Box::new(SExpr::Operation(|mut $cxt: &mut Context| {
                    use SExpr::*;
                    $impl
                })),
                Box::new(parse::parse($ptn)),
            ),
        )
    };
}

impl Bindings {
    fn join(mut self, other: Bindings) -> Bindings {
        self.0.extend(other.0.into_iter());
        self
    }

    fn new() -> Bindings {
        Bindings(HashMap::new())
            .join(primitive!(
                "#/add",
                "(,lhs ,rhs)",
                Int(get!("lhs", cxt).as_int().unwrap() + get!("rhs", cxt).as_int().unwrap()),
                cxt
            ))
            .join(primitive!(
                "#/bind-expr",
                "(,ident ,expr)",
                {
                    cxt.bindings.0.insert(
                        get!("ident", &mut cxt.clone()).as_ident().unwrap(),
                        get!("expr", &mut cxt.clone()),
                    );
                    List(vec![])
                },
                cxt
            ))
            .join(primitive!(
                "#/do",
                ",exprs",
                {
                    match get!("exprs", &mut cxt.clone()) {
                        List(exprs) => {
                            for expr in exprs {
                                let _ = expr.eval(&mut cxt);
                            }
                        }
                        _ => unreachable!(),
                    }
                    List(vec![])
                },
                cxt
            ))
            .join(primitive!(
                "#/do/ret-all",
                "(,exprs)",
                {
                    let mut results = Vec::new();
                    match get!("exprs", &mut cxt.clone()) {
                        List(exprs) => {
                            for expr in exprs {
                                results.push(expr.eval(&mut cxt));
                            }
                        }
                        _ => unreachable!(),
                    }
                    List(results)
                },
                cxt
            ))
            .join(primitive!(
                "#/eq", //this will probably be not a primitive later
                "(,lhs ,rhs)",
                {
                    if get!("lhs", &mut cxt.clone()) == get!("rhs", &mut cxt.clone()) {
                        Keyword(crate::Ident::Name("true".to_string()))
                    } else {
                        Keyword(crate::Ident::Name("false".to_string()))
                    }
                },
                cxt
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
        self.bindings.0.get(&ident).cloned()
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
            Unquote => write!(f, "Unquote"),
            CompileError(e) => write!(f, "CompileError({:?})", e),
            Fun(fun, p) => write!(f, "Fun({:?}, {:?})", *fun, *p),
            Int(i) => write!(f, "Int({:?})", i),
            Operation(_) => write!(f, "Operation"),
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

fn main() {
    dbg!(parse::parse(
        "(#/do/ret-all `(`(#/bind-expr `foo 3) `foo)))"
    ));
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
    eval_test! {simple_do, "(#/do 1 (#/add 1 2))", List(vec![])}
    eval_test! {define_and_use, "(#/do/ret-all `((#/bind-expr `foo 3) foo))", List(vec![
        List(vec![]),
        Int(3)
    ])}
    eval_test! {eq, "(#/eq 1 1)", Keyword(super::Ident::Name("true".to_string()))}
    eval_test! {neq, "(#/eq 1 2)", Keyword(super::Ident::Name("false".to_string()))}
    eval_test! {eq_lists, "(#/eq `(1 2) `(1 `(2 3)))", Keyword(super::Ident::Name("false".to_string()))}
}
