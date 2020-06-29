//! A simple unoptimized interpreter for lispap (LISt and PAttern Programming)
//! the goal is the simplest possible implementation, for bootstrapping

#![cfg_attr(not(test), allow(dead_code))]
#![feature(hash_set_entry)]

#[macro_use]
mod macros;
mod context;
mod intern;
mod parse;

use itertools::merge;
use lazy_static::lazy_static;

use std::fmt::Debug;
use std::iter;

use crate::context::{Bindings, Context};
use crate::intern::{Interned, Interner};

lazy_static! {
    static ref IDENTS: Interner<Ident> = Interner::new();
}

#[derive(Clone)]
pub enum SExpr {
    Sigil(char),
    List(Vec<SExpr>),
    Ident(Interned<'static, Ident>),
    Place(Interned<'static, Ident>),
    Fun(Box<SExpr>, Box<SExpr>, Box<Bindings>),
    UnarySigilApp(char, Box<SExpr>),
    Int(isize),
    Operation(fn(&mut Context) -> SExpr),
    Keyword(Interned<'static, Ident>),
    PtnUnion(Box<SExpr>, Box<SExpr>),
    PtnIntersect(Box<SExpr>, Box<SExpr>),
    Spread(Vec<SExpr>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    names: Vec<String>,
    tl_ns: bool,
}

impl SExpr {
    fn eval(&self, mut cxt: &mut Context, debug: bool) -> SExpr {
        use SExpr::*;
        if debug {
            dbg!(self);
        }
        let mut self_simp = self.clone();
        self_simp.simplify();
        let mut expr = match self_simp {
            List(ls) => {
                if ls.is_empty() {
                    panic!("Tried to evaluate an empty list");
                }
                match ls[0].clone().eval(cxt, debug) {
                    Fun(fun, args_ptn, fun_bindings) => {
                        if let Some(bindings) = args_ptn.match_ptn(&List(
                            ls[1..]
                                .iter()
                                .map(|e| e.eval(&mut cxt, debug))
                                .collect::<Vec<_>>(),
                        )) {
                            cxt.push_scope();
                            cxt.add_bindings(&fun_bindings);
                            cxt.push_scope();
                            cxt.add_bindings(&bindings);
                            let expr = fun.eval(&mut cxt, debug);
                            cxt.pop_scope();
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
                if let Fun(fun, args_ptn, fun_bindings) = Sigil(sigil.clone()).eval(&mut cxt, debug)
                {
                    if let Some(bindings) = args_ptn.match_ptn(&List(vec![*arg.clone()])) {
                        cxt.push_scope();
                        cxt.add_bindings(&fun_bindings);
                        cxt.push_scope();
                        cxt.add_bindings(&bindings);
                        let expr = fun.eval(&mut cxt, debug);
                        cxt.pop_scope();
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
                .lookup(id)
                .unwrap_or_else(|| panic!("Unknown name {:?}", id)),
            Operation(oper) => oper(&mut cxt),
            Sigil(s) => cxt
                .lookup(make_sigil_ident(s))
                .unwrap_or_else(|| panic!("Use of undefined sigil {}", s)),
            Spread(_) => panic!("tried to eval a spread"),
            e => e.clone(),
        };
        if debug {
            dbg!(&expr);
        }
        expr.simplify();
        expr
    }

    fn match_ptn(&self, expr: &SExpr) -> Option<Bindings> {
        use SExpr::*;
        match (self, expr) {
            (PtnUnion(a, b), expr) => dbg!(a)
                .match_ptn(expr)
                .map(|bind| bind.join(&b.match_ptn(expr).unwrap_or(Bindings::empty()))),
            (PtnIntersect(a, b), expr) => dbg!(a)
                .match_ptn(expr)
                .and_then(|bind| b.match_ptn(expr).as_ref().map(|bind2| bind.join(bind2))),
            (Keyword(id1), Keyword(id2)) | (Ident(id1), Ident(id2)) if *id1 == *id2 => {
                Some(Bindings::empty())
            }
            (Fun(_, _, _), Fun(_, _, _)) => None,
            (List(left), List(right)) => {
                if left.len() != right.len() {
                    return None;
                }
                let mut bindings = Some(Bindings::empty());
                for (left, right) in left.iter().zip(right.iter()) {
                    match left.match_ptn(right) {
                        Some(bind) => bindings = bindings.map(|b| b.join(&bind)),
                        None => bindings = None,
                    }
                }
                bindings
            }
            (Int(i1), Int(i2)) if i1 == i2 => Some(Bindings::empty()),
            (Place(id), thing) => Some(Bindings::of(*id, thing)),
            _ => None,
        }
    }

    fn quote(self, mut cxt: &mut Context) -> SExpr {
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

    fn simplify(&mut self) {
        use SExpr::*;
        *self = match &*self {
            List(ls) => {
                let mut simp_ls: Vec<SExpr> = Vec::new();
                for expr in ls {
                    let mut expr = expr.clone();
                    expr.simplify();
                    if let Spread(exprs) = expr {
                        simp_ls.extend(exprs.into_iter())
                    } else {
                        simp_ls.push(expr)
                    }
                }
                List(simp_ls)
            }
            e => e.clone(),
        }
        .clone();
    }

    fn referenced_idents(&self) -> Vec<Interned<'static, Ident>> {
        let mut idents = self.referenced_idents_inner();
        idents.dedup();
        idents
    }

    fn referenced_idents_inner(&self) -> Vec<Interned<'static, Ident>> {
        use SExpr::*;
        match self {
            Ident(id) | Place(id) | Keyword(id) => vec![*id],
            Sigil(sig) => vec![make_sigil_ident(*sig)],
            Fun(a, b, _) | PtnUnion(a, b) | PtnIntersect(a, b) => {
                merge(a.referenced_idents_inner(), b.referenced_idents_inner()).collect::<Vec<_>>()
            }
            UnarySigilApp(sig, arg) => merge(
                iter::once(make_sigil_ident(*sig)),
                arg.referenced_idents_inner(),
            )
            .collect::<Vec<_>>(),
            List(ls) | Spread(ls) => {
                let mut idents = ls
                    .iter()
                    .flat_map(Self::referenced_idents_inner)
                    .collect::<Vec<_>>();
                idents.sort();
                idents
            }
            Int(_) | Operation(_) => vec![],
        }
    }

    fn as_int(self) -> Option<isize> {
        if let SExpr::Int(i) = self {
            Some(i)
        } else {
            None
        }
    }

    fn as_ident(self) -> Option<Interned<'static, Ident>> {
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

fn make_sigil_ident(sigil: char) -> Interned<'static, Ident> {
    let name = match sigil {
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
    };
    ident!(&format!("#/sigil/{}", name))
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
            (Fun(_, _, _), Fun(_, _, _)) => false,
            (Int(i0), Int(i1)) if i0 == i1 => true,
            (PtnUnion(a1, b1), PtnUnion(a2, b2)) if a1 == a2 && b1 == b2 => true,
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
            PtnUnion(a, b) => f.debug_tuple("PtnUnion").field(a).field(b).finish(),
            PtnIntersect(a, b) => f.debug_tuple("PtnUnion").field(a).field(b).finish(),
            Spread(exprs) => {
                write!(f, "Spread")?;
                f.debug_list().entries(exprs.iter()).finish()
            }
            Keyword(id) => write!(f, "Keyword({:?})", id),
            Ident(id) => write!(f, "Ident({:?})", id),
            Place(id) => write!(f, "Place({:?})", id),
            Fun(fun, p, _) => write!(f, "Fun({:?}, {:?}, _)", *fun, *p),
            Int(i) => write!(f, "Int({:?})", i),
            Operation(_) => write!(f, "Operation"),
            Sigil(s) => write!(f, "Sigil({})", s),
        }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        if self.tl_ns {
            write!(f, "/")?;
        }
        write!(f, "{}", self.names[0])?;
        for name in self.names.iter().skip(1) {
            write!(f, "/{}", name)?;
        }
        Ok(())
    }
}

fn main() {
    use SExpr::*;
    let code = lispap!(&format!("[{} {}]", *LISPAP_STD_STR, "(fib 5)"));
    assert_eq!(
        *code
            .eval(&mut Context::new(), true)
            .as_list()
            .unwrap()
            .last()
            .unwrap(),
        Int(5)
    );
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

    eval_test! {lone_number, "5", Int(5)}
    eval_test! {neg_number, "-5", Int(-5)}
    eval_test! {one_plus_one, "(#/add 1 1)", Int(2)}
    eval_test! {one_plue_one_plus_one, "(#/add 1 (#/add 1 1))", Int(3)}
    eval_test! {multiple_levels_ident, "`foo/bar/baz", Ident(IDENTS.intern(crate::Ident{
        names: vec!["foo".to_string(), "bar".to_string(), "baz".to_string()],
        tl_ns: false
    }))}
    eval_test! {quote, "`(1 (#/add 2 3))", List(vec![
        Int(1),
        List(vec![
            Ident(ident!("#/add")),
            Int(2),
            Int(3),
        ]),
    ])}
    eval_test! {simple_do, "[(#/add 1 2)]", List(vec![Int(3)])}
    eval_test! {eq, "(#/eq 1 1)", Keyword(ident!("true"))}
    eval_test! {neq, "(#/eq 1 2)", Keyword(ident!("false"))}
    eval_test! {eq_lists, "(#/eq `(1 2) `(1 `(2 3)))",
                Keyword(ident!("false"))
    }
    eval_test_std! {uses_std, "std-is-here", Int(42)}
    eval_test_std! {fib_in_std, "(fib 4)", Int(3)}
    eval_test! {list_item_after_sublist, "(#/add (#/add 1 2) 3)", Int(6)}
    eval_test_std! {id_int, "(id 42)", Int(42)}
    eval_test! {sq_brkt, "[,foo]", List(vec![Place(ident!("foo"))])}
    eval_test_std! {def, "(def ,foo 123) foo", Int(123)}
    eval_test_std! {std_works, "3", Int(3)}
    eval_test_std! {sigil_as_value, "(: `foo)", Keyword(ident!("foo"))}
    eval_test_std! {ptn_union_create, "(~ 4 ,foo)",
                    PtnUnion(Box::new(Int(4)), Box::new(Place(ident!("foo"))))
    }
    eval_test_std! {ptn_intersect, "(with? (^ 4 ,foo) 4 `foo never)", Int(4)}
    eval_test_std! {ptn_intersect_not_matching,
                    "(with? (^ 4 ,foo) 5 never unit)",
                    Keyword(ident!("unit"))
    }
    eval_test_std! {spread, "[1 2 &[3 4] 5 6]",
                    List(vec![Int(1), Int(2), Int(3), Int(4), Int(5), Int(6)])
    }
    eval_test_std! {spread_1, "[1 2 &[3]]", lispap!("(1 2 3)")}
    eval_test_std! {map_id, "(list/map id [1 2 3 4 5])",
                    List(vec![Int(1), Int(2), Int(3), Int(4), Int(5)])
    }
    eval_test_std! {map_id_0, "(list/map id [])", List(vec![])}
    eval_test_std! {map_id_1, "(list/map id [1])", List(vec![Int(1)])}
    eval_test_std! {head_1, "(list/head [1])", Int(1)}
    eval_test_std! {tail, "(list/tail [1 2 3])", List(vec!(Int(2), Int(3)))}
    eval_test_std! {tail_1, "(list/tail [1])", List(vec![])}
    eval_test_std! {tail_0, "(list/tail [])", List(vec![])}
    eval_test_std! {spread_empty, "[1 &[] &[]]", List(vec![Int(1)])}

    #[test]
    fn context() {
        let _ = Context::new();
    }
}
