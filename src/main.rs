//! A simple unoptimized interpreter for Patter
//! the goal is the simplest possible implementation, for bootstrapping

#![cfg_attr(not(test), allow(dead_code))]
#![feature(hash_set_entry, try_blocks, bindings_after_at)]

#[macro_use]
mod macros;
mod context;
mod error;
mod intern;
mod parse;

use itertools::merge;
use lazy_static::lazy_static;

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::iter;

use crate::context::{Bindings, Context};
use crate::error::InterpreterError;
use crate::intern::{Interned, Interner};

lazy_static! {
    static ref IDENTS: Interner<Ident> = Interner::new();
}

lazy_static! {
    static ref STD_CXT: Context = {
        let mut cxt = Context::new();
        patter!(&format!("[{}]", *PATTER_STD_STR))
            .eval(&mut cxt)
            .unwrap();
        cxt
    };
}

#[derive(Clone)]
pub enum SExpr {
    Sigil(char),
    List(Vec<SExpr>),
    Ident(Interned<'static, Ident>),
    Place(Interned<'static, Ident>),
    Fun(Fun),
    UnarySigilApp(char, Box<SExpr>),
    Int(isize),
    Operation {
        eval: fn(&mut Context) -> Result<SExpr, InterpreterError>,
        evals_to:
            fn(&dyn Fn(Interned<'static, Ident>) -> Option<SExpr>) -> SExpr,
    },
    PtnAcc {
        acc: Fun,
        init: Option<Bindings>,
        pats: Vec<SExpr>,
    },
    Consecutive(Vec<SExpr>),
    Spread(Vec<SExpr>),
    Kleene {
        start: Box<SExpr>,
        next: Fun,
    },
    AtPtnTime(Box<SExpr>),
    LitMatch(Box<SExpr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum SExprKind {
    Sigil,
    List,
    Ident,
    Place,
    Fun,
    UnarySigilApp,
    Int,
    Operation,
    Keyword,
    Spread,
    Rest,
    AtPtnTime,
    PtnAcc,
    LitMatch,
    Consecutive,
    Kleene,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    names: Vec<String>,
    tl_ns: bool,
}

#[derive(Clone, Debug)]
pub struct Fun {
    body: Box<SExpr>,
    args_ptn: Box<SExpr>,
    closure: Box<Bindings>,
}

impl SExpr {
    fn eval(&self, mut cxt: &mut Context) -> Result<SExpr, InterpreterError> {
        use SExpr::*;
        let mut self_simp = self.clone();
        self_simp.simplify();
        let result: Result<SExpr, InterpreterError> = try {
            let mut expr = match self_simp {
                List(ls) => {
                    if ls.is_empty() {
                        throw_interpreter_err!(
                            CannotEvaluate,
                            SExpr::List(vec![])
                        )
                    }
                    ls[0]
                        .clone()
                        .eval(cxt)?
                        .as_fun()
                        .ok_or(interpreter_err!(CannotCall, ls[0].clone()))?
                        .call(
                            ls[1..]
                                .iter()
                                .map(|e| e.eval(&mut cxt))
                                .collect::<Result<_, _>>()?,
                            cxt,
                        )?
                }
                UnarySigilApp(sigil, arg) => {
                    let fun = Sigil(sigil.clone()).eval(&mut cxt)?;
                    fun.clone()
                        .as_fun()
                        .ok_or(interpreter_err!(CannotCall, fun))?
                        .call(vec![*arg], cxt)?
                }
                Ident(id) => {
                    cxt.lookup(id).ok_or(interpreter_err!(UnknownName, id))?
                }
                Operation { eval, .. } => eval(&mut cxt)?,
                Sigil(s) => cxt
                    .lookup(make_sigil_ident(s))
                    .ok_or(interpreter_err!(UndefinedSigil, s))?,
                e @ Spread(_)
                | e @ Consecutive(_)
                | e @ Kleene { .. }
                | e @ LitMatch(_)
                | e @ Place(_)
                | e @ PtnAcc { .. }
                | e @ Fun(_)
                | e @ AtPtnTime(_) => {
                    throw_interpreter_err!(CannotEvaluate, e);
                    unreachable!();
                }
                s @ Int(_) => s,
            };
            expr.simplify();
            expr
        };
        result.map_err(|mut e| {
            e.callstack.push(format!("While evaluating {:#?}", self));
            e
        })
    }

    fn match_ptn(
        &self,
        expr: &SExpr,
    ) -> Result<Option<Bindings>, InterpreterError> {
        use SExpr::*;
        let result: Result<Option<Bindings>, InterpreterError> = try {
            match (self, expr) {
                (pat, expr) if pat.matches_literally() => {
                    if pat == expr {
                        Some(Bindings::empty())
                    } else {
                        None
                    }
                }
                (List(left), List(right)) => match (&**left, &**right) {
                    ([], []) => Some(Bindings::empty()),
                    ([], _) => None,
                    ([ref pat, ..], []) if pat.matches_singular() => None,
                    (&[.., ref pat], &[.., ref expr])
                        if pat.matches_singular() =>
                    {
                        match (
                            pat.match_ptn(&expr)?,
                            List(left[..(left.len() - 1)].to_vec()).match_ptn(
                                &List(right[..(right.len() - 1)].to_vec()),
                            )?,
                        ) {
                            (Some(left_binds), Some(right_binds)) => {
                                Some(left_binds.join(&right_binds))
                            }
                            _ => None,
                        }
                    }
                    (&[ref pat, ..], &[ref expr, ..])
                        if pat.matches_singular() =>
                    {
                        match (
                            pat.match_ptn(&expr)?,
                            List(left[1..].to_vec())
                                .match_ptn(&List(right[1..].to_vec()))?,
                        ) {
                            (Some(left_binds), Some(right_binds)) => {
                                Some(left_binds.join(&right_binds))
                            }
                            _ => None,
                        }
                    }
                    (left @ [Kleene{ start, next }, ..], exprs) => {
                        println!("Matching against a kleene: {:?}", exprs);
                        let mut out_binds = Some(Bindings::basic()); //dbg
                        let mut pats = start.clone().as_list().unwrap();
                        for i in (pats.len())..=exprs.len() {
                            debug_assert!(pats.len() == i);
                            println!("Pats: {:?}", pats);
                            // this is wrong for kleenes of consecs or kleens
                            if let (Some(left), Some(right)) =  (
                                List(pats.clone()).match_ptn(
                                    &List(exprs[..i].to_vec())
                                )?,
                                List(left[1..].to_vec())
                                    .match_ptn(&List(exprs[i..].to_vec()))?
                            ) {
                                out_binds = Some(left.join(&right));
                            }
                            pats.push(next.call(
                                vec![List(pats.clone())],
                                &mut Context::empty()
                            )?);
                        }
                        dbg!(out_binds)
                    }
                    (left @ [Consecutive(pats),..], exprs) => {
                        match (
                            List(pats.to_vec()).match_ptn(&List(exprs[..pats.len()].to_vec()))?,
                            List(left[1..].to_vec()).match_ptn(&List(exprs[pats.len()..].to_vec()))?,
                        ){
                            (Some(left), Some(right)) => Some(left.join(&right)),
                            _ => None
                        }
                    }
                    (pats, exprs) => panic!(
                        "Failed to handle pattern match on List{:?} of List{:?}",
                        pats, exprs
                    ),
                },
                (List(_), _) => None,
                (AtPtnTime(pat), thing) => pat
                    .clone()
                    .as_fun()
                    .ok_or(interpreter_err!(CannotCall, *pat.clone()))?
                    .call(vec![], &mut Context::empty())?
                    .match_ptn(thing)?,
                (Place(id), thing) => Some(Bindings::of(*id, thing)),
                (PtnAcc { acc, init, pats }, expr) => {
                    let mut bindings = init.clone();
                    for pat in pats {
                        bindings = Option::<Bindings>::from_sexpr(patter_sr!(
                            acc,
                            SExpr::List(vec![
                                bindings.into_sexpr(),
                                pat.match_ptn(expr)?.into_sexpr(),
                            ])
                        )?)?;
                    }
                    bindings
                }
                (UnarySigilApp(l_sig, l_arg), UnarySigilApp(r_sig, r_arg)) => {
                    if l_sig == r_sig {
                        l_arg.match_ptn(r_arg)?
                    } else {
                        None
                    }
                }
                (UnarySigilApp(_,_), _) => None,
                (a, b) => panic!("Unhandled pattern match: {:?}, {:?}", a, b),
            }
        };
        /*
                println!("Matched:");
                println!("Pattern: {:#?}", self);
                println!("Value: {:#?}", expr);
                println!("Result: {:#?}", result);
                println!("=====");
        */
        result.map_err(|mut e| {
            e.callstack.push(format!(
                "While matching {:#?} against {:#?}",
                self, expr
            ));
            e
        })
    }

    fn matches_singular(&self) -> bool {
        use SExpr::*;
        match self {
            Sigil(_)
            | Ident(_)
            | LitMatch(_)
            | List(_)
            | Place(_)
            | Fun(_)
            | UnarySigilApp(_, _)
            | Int(_)
            | Operation { .. } => true,
            PtnAcc { pats, .. } => pats.iter().all(|p| p.matches_singular()),
            Consecutive(_) | Kleene { .. } | AtPtnTime(_) => false,
            Spread(_) => unreachable!(),
        }
    }

    fn matches_literally(&self) -> bool {
        use SExpr::*;
        match self {
            Sigil(_) | Ident(_) | Int(_) | Operation { .. } => true,
            List(ls) => ls.iter().all(|e| e.matches_literally()),
            Place(_)
            | Fun(_)
            | UnarySigilApp(_, _)
            | PtnAcc { .. }
            | Consecutive(_)
            | Kleene { .. }
            | AtPtnTime(_)
            | LitMatch(_) => false,
            Spread(_) => unreachable!(),
        }
    }

    fn evals_to(&self) -> SExpr {
        unimplemented!();
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
            Ident(id) | Place(id) => vec![*id],
            Sigil(sig) => vec![make_sigil_ident(*sig)],
            Fun(crate::Fun {
                body: a,
                args_ptn: b,
                ..
            }) => {
                merge(a.referenced_idents_inner(), b.referenced_idents_inner())
                    .collect::<Vec<_>>()
            }
            LitMatch(expr) | AtPtnTime(expr) => expr.referenced_idents_inner(),
            UnarySigilApp(sig, arg) => merge(
                iter::once(make_sigil_ident(*sig)),
                arg.referenced_idents_inner(),
            )
            .collect::<Vec<_>>(),
            List(ls) | Spread(ls) | Consecutive(ls) => {
                let mut idents = ls
                    .iter()
                    .flat_map(Self::referenced_idents_inner)
                    .collect::<Vec<_>>();
                idents.sort();
                idents
            }
            PtnAcc { acc, pats, init } => merge(
                Fun(acc.clone()).referenced_idents_inner(),
                merge(
                    init.clone()
                        .map(|b| b.referenced_idents_sorted())
                        .unwrap_or(Vec::new()),
                    List(pats.to_vec()).referenced_idents_inner(),
                ),
            )
            .collect::<Vec<_>>(),
            Kleene { start, next } => merge(
                start.referenced_idents_inner(),
                Fun(next.clone()).referenced_idents_inner(),
            )
            .collect(),
            Int(_) | Operation { .. } => vec![],
        }
    }

    fn kind(&self) -> SExprKind {
        use SExprKind::*;
        match self {
            SExpr::Sigil(_) => Sigil,
            SExpr::List(_) => List,
            SExpr::Ident(_) => Ident,
            SExpr::Spread(_) => Spread,
            SExpr::Place(_) => Place,
            SExpr::Fun(_) => Fun,
            SExpr::UnarySigilApp(_, _) => UnarySigilApp,
            SExpr::Int(_) => Int,
            SExpr::Operation { .. } => Operation,
            SExpr::AtPtnTime(_) => AtPtnTime,
            SExpr::PtnAcc { .. } => PtnAcc,
            SExpr::LitMatch(_) => LitMatch,
            SExpr::Consecutive(_) => Consecutive,
            SExpr::Kleene { .. } => Kleene,
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

    fn as_sigil(self) -> Option<char> {
        if let SExpr::Sigil(sig) = self {
            Some(sig)
        } else {
            None
        }
    }

    fn as_fun(self) -> Option<Fun> {
        if let SExpr::Fun(fun) = self {
            Some(fun)
        } else {
            None
        }
    }

    fn as_solidified(self) -> Option<SExpr> {
        if let SExpr::UnarySigilApp(':', thing) = self {
            Some(*thing)
        } else {
            None
        }
    }
}

impl Fun {
    fn call(
        &self,
        args: Vec<SExpr>,
        mut cxt: &mut Context,
    ) -> Result<SExpr, InterpreterError> {
        if let Some(bindings) =
            self.args_ptn.match_ptn(&SExpr::List(args.clone()))?
        {
            cxt.push_scope();
            cxt.add_bindings(&self.closure);
            cxt.push_scope();
            cxt.add_bindings(&bindings);
            let expr = self.body.eval(&mut cxt);
            cxt.pop_scope();
            cxt.pop_scope();
            expr
        } else {
            throw_interpreter_err!(NonMatchingArgs, self.clone(), args)
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
            (List(v0), List(v1)) => v0 == v1,
            (Ident(id0), Ident(id1)) | (Place(id0), Place(id1)) => id0 == id1,

            (Fun(_), Fun(_)) => false,
            (Int(i0), Int(i1)) => i0 == i1,
            (UnarySigilApp(sig1, expr1), UnarySigilApp(sig2, expr2)) => {
                sig1 == sig2 && expr1 == expr2
            }
            (Sigil(s1), Sigil(s2)) => s1 == s2,
            (a, b) if a.kind() != b.kind() => false,
            (a, b) => panic!("Unhandled equality case: ({:?}, {:?})", a, b),
        }
    }
}

impl Debug for SExpr {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), std::fmt::Error> {
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
            AtPtnTime(expr) => f.debug_tuple("AtPtnTime").field(expr).finish(),
            Spread(exprs) => {
                write!(f, "Spread")?;
                f.debug_list().entries(exprs.iter()).finish()
            }
            Ident(id) => write!(f, "Ident({:?})", id),
            Place(id) => write!(f, "Place({:?})", id),
            Fun(fun) => f.debug_tuple("Fun").field(fun).finish(),
            Int(i) => write!(f, "Int({:?})", i),
            Operation { .. } => write!(f, "Operation"),
            Sigil(s) => write!(f, "Sigil({})", s),
            PtnAcc { acc, init, pats } => f
                .debug_struct("PtnAcc")
                .field("acc", acc)
                .field("init", init)
                .field("pats", pats)
                .finish(),
            LitMatch(expr) => f.debug_tuple("LitMatch").field(expr).finish(),
            Consecutive(exprs) => {
                write!(f, "Consecutive")?;
                f.debug_list().entries(exprs.iter()).finish()
            }
            Kleene { start, next } => f
                .debug_struct("Kleene")
                .field("start", start)
                .field("next", next)
                .finish(),
        }
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Ident {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), std::fmt::Error> {
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
/*
impl Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<FUN: BODY={} ARGS={} CLOSURE={}>",
            self.body, self.args_ptn, self.closure
        )
    }
}
*/
/*
impl Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SExpr::*;
        match self {
            Sigil(sig) => write!(f, "{}", sig)?,
            List(ls) => {
                write!(f, "(")?;
                for expr in ls {
                    write!(f, " {}", expr)?;
                }
                write!(f, ")")?;
            }
            Fun(fun) => write!(f, "{}", fun)?,
            Ident(id) => write!(f, "{}", id)?,
            AtPtnTime(expr) => write!(f, "<AT-PTN-TIME {}>", expr)?,
            Place(id) => write!(f, "<PLACE {}>", id)?,
            UnarySigilApp(sig, arg) => write!(f, "{}{}", sig, arg)?,
            Int(i) => write!(f, "{}", i)?,
            Operation { .. } => write!(f, "<OPERATION>")?,
            Keyword(id) => write!(f, "<KEYWORD {}>", id)?,
            Spread(ls) => write!(f, "<SPREAD {}>", List(ls.clone()))?,
            Rest(expr) => write!(f, "<REST {}>", expr)?,

        }
        Ok(())
    }
}
 */

trait FromSExpr: Sized {
    fn from_sexpr(_: SExpr) -> Result<Self, InterpreterError>;
}

trait IntoSExpr {
    fn into_sexpr(self) -> SExpr;
}

impl IntoSExpr for Fun {
    fn into_sexpr(self) -> SExpr {
        SExpr::Fun(self)
    }
}

impl FromSExpr for Fun {
    fn from_sexpr(expr: SExpr) -> Result<Fun, InterpreterError> {
        expr.clone().as_fun().ok_or(interpreter_err!(
            CannotConvert,
            "Not a Fun",
            expr
        ))
    }
}

impl FromSExpr for Bindings {
    fn from_sexpr(expr: SExpr) -> Result<Bindings, InterpreterError> {
        let result: Result<Bindings, InterpreterError> = try {
            Bindings::of_contents(
                expr.clone()
                    .as_list()
                    .ok_or(interpreter_err!(
                        CannotConvert,
                        "Not a list",
                        expr.clone()
                    ))?
                    .iter()
                    .map(|ls| {
                        let pair =
                            ls.clone().as_list().ok_or(interpreter_err!(
                                CannotConvert,
                                "Not a pair",
                                ls.clone()
                            ))?;
                        if pair.len() != 2 {
                            throw_interpreter_err!(
                                CannotConvert,
                                "Not a pair",
                                ls.clone()
                            )
                        }
                        Ok((
                            pair[0]
                                .clone()
                                .as_solidified()
                                .and_then(|e| e.as_ident())
                                .ok_or(interpreter_err!(
                                    CannotConvert,
                                    "Not a solidifed ident",
                                    pair[0].clone()
                                ))?,
                            pair[1].clone(),
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            )
        };
        result.map_err(|mut e| {
            e.callstack
                .push(format!("While converting into a bindings: {:#?}", expr));
            e
        })
    }
}

impl<T: IntoSExpr> IntoSExpr for Option<T> {
    fn into_sexpr(self) -> SExpr {
        match self {
            Some(it) => SExpr::List(vec![
                patter_std!(":some").unwrap(),
                it.into_sexpr(),
            ]),
            None => SExpr::List(vec![
                patte_std!(":none").unwrap()
            ]),
        }
    }
}

impl<T: FromSExpr> FromSExpr for Option<T> {
    fn from_sexpr(expr: SExpr) -> Result<Option<T>, InterpreterError> {
        let result: Result<Option<T>, InterpreterError> = try {
            let ls = expr.clone().as_list().ok_or(interpreter_err!(
                NotA,
                SExprKind::List,
                expr.clone()
            ))?;
            if ls.len() == 0 || ls.len() > 2 {
                throw_interpreter_err!(
                    CannotConvert,
                    "Options must be of len 1 or 2",
                    expr.clone()
                )
            }
            let discr = ls[0].clone();
            if discr == patter_std!(":some")? {
                Some(T::from_sexpr(ls[1].clone())?)
            } else if discr == patter_std!(":none")? {
                None
            } else {
                throw_interpreter_err!(
                    CannotConvert,
                    "Unknonw discriminant",
                    discr
                )
            }
        };
        result.map_err(|mut e| {
            e.callstack
                .push(format!("While converting into an Option: {:#?}", expr.clone()));
            e
        })
    }
}

fn main() -> Result<(), InterpreterError> {
    #[allow(unused_imports)]
    use SExpr::*;
    dbg!(Some(Bindings::empty()).into_sexpr());
    /*
        let ptn = {
            let test_code = "(^ 4 ,foo)";
            let code = patter!(&format!("[{} {}]", *PATTER_STD_STR, test_code));
            let result = code
                .eval(&mut Context::new())?
                .as_list()
                .unwrap()
                .last()
                .unwrap()
                .clone();
            println!("Result: {:#?}", result);
            result
        };
        let value = {
            let test_code = "[[:some []] [:some []]]";
            let code = patter!(&format!("[{} {}]", *PATTER_STD_STR, test_code));
            let result = code
                .eval(&mut Context::new())?
                .as_list()
                .unwrap()
                .last()
                .unwrap()
                .clone();
            //        println!("Result: {:#?}", result);
            result
        };
        dbg!(ptn.match_ptn(&value).unwrap());
    */
    Ok(())
}

lazy_static! {
    static ref PATTER_STD_STR: String =
        std::fs::read_to_string("patter_std/std.pat").unwrap();
}

#[cfg(test)]
mod tests {

    macro_rules! eval_test {
        ($name:ident, $code:expr, $expected:expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    patter!($code).eval(&mut Context::new()).unwrap(),
                    $expected
                );
            }
        };
    }

    macro_rules! eval_test_std {
        ($name:ident, $code:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let code = patter!(&format!("[{} {}]", *PATTER_STD_STR, $code));
                assert_eq!(
                    *code
                        .eval(&mut Context::new())
                        .unwrap_or_else(|e| panic!("Error: {}", e))
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
    eval_test! {
        multiple_levels_ident,
        "`foo/bar/baz",
        Ident(IDENTS.intern(crate::Ident{
            names: vec!["foo".to_string(), "bar".to_string(), "baz".to_string()],
            tl_ns: false
        }))
    }

    eval_test! {quote, "`(1 (#/add 2 3))", List(vec![
        Int(1),
        List(vec![
            Ident(ident!("#/add")),
            Int(2),
            Int(3),
        ]),
    ])}

    eval_test! {
        simple_do,
        "[(#/add 1 2)]",
        List(vec![Int(3)])
    }

    eval_test_std! {uses_std, "std-is-here", Int(42)}
    eval_test_std! {fib_in_std, "(fib 4)", Int(3)}
    eval_test! {list_item_after_sublist, "(#/add (#/add 1 2) 3)", Int(6)}
    eval_test_std! {id_int, "(id 42)", Int(42)}
    eval_test! {sq_brkt, "[,foo]", List(vec![Place(ident!("foo"))])}
    eval_test_std! {def, "(def ,foo 123) foo", Int(123)}
    eval_test_std! {std_works, "3", Int(3)}
    eval_test_std! {sigil_as_value, "(` `foo)", Ident(ident!("foo"))}
    /*    eval_test_std! {ptn_union_create, "(~ 4 ,foo)",
                        PtnUnion(Box::new(Int(4)), Box::new(Place(ident!("foo"))))
        }
    */
    eval_test_std! {ptn_intersect, "(with? (^ 4 ,foo) 4 `foo never)", Int(4)}
    eval_test_std! {
        ptn_intersect_not_matching,
        "(with? (^ 4 ,foo) 5 never unit)",
        patter_std!("unit").unwrap()
    }
    eval_test_std! {
        ptn_union,
        "(with? (~ 3 4) 3 unit never)",
        patter_std!("unit").unwrap()
    }
    eval_test_std! {spread, "[1 2 &[3 4] 5 6]",
                    List(vec![Int(1), Int(2), Int(3), Int(4), Int(5), Int(6)])
    }
    eval_test_std! {spread_1, "[1 2 &[3]]", patter!("(1 2 3)")}
    eval_test_std! {spread_2_spreads, "[&[1 2] &[1 2]]", patter!("(1 2 1 2)")}
    eval_test_std! {spread_nested, "[&[[1 2] [3 4]] [5 6]]", patter!("((1 2) (3 4) (5 6))")}
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
    eval_test_std! {
        solidify,
        "(id (id (id (id (id :foo)))))",
        patter_std!(":foo").unwrap()
    }
    eval_test_std! {melt, "(melt :foo)", Ident(ident!("foo"))}
    eval_test_std! {
        default_args,
        "(with? default-args [3 5] `(#/add '0 '1) never)",
        Int(8)
    }
    eval_test_std! {dedup, "(list/dedup [1 2 3 3 4 6 7 1 3])", patter!("(1 2 3 4 6 7)")}
    eval_test_std! {
        dedup_bindings,
        "(list/dedup [[`a 1] [`b 2] [`c 3] [`d 4]])",
        patter!("((a 1) (b 2) (c 3) (d 4))")
    }
    eval_test_std! {
        contains,
        "(list/contains [[`a 1]] [`b 2])",
        patter_std!(":false").unwrap()
    }
    eval_test_std! {bindings_join, "(bindings/join [[`a 1] [`b 2]] [[`c 3] [`d 4]])",
                    patter!("((a 1) (b 2) (c 3) (d 4))")
    }
    eval_test_std! {
        match_binding,
        "(with? [`a 1] [`b 2] :true :false)",
        patter_std!(":false").unwrap()
    }
    eval_test_std! {any, "(with? any [ 1 3 [ [] [] :hi]] 1 never)", Int(1)}
    eval_test_std! {
        kleene,
        "(with? [(many any)] [1 2 [] 5 10 :foo] `unit `never)",
        patter_std!("unit").unwrap()
    }
    eval_test_std! {
        kleene_with_end,
        "(with? [(many any) :foo] [1 2 [] :foo [] [:foo] 3 4 :foo] `unit `never)",
        patter_std!("unit").unwrap()
    }
    eval_test_std! {
        kleene_with_end_place,
        "(with? [(many any) ,foo] [1 2 3 4] `foo `never)",
        Int(4)
    }
    eval_test_std! {
        kleene_split,
        "(with? [(many any) :foo (many any)] [1 2 :foo 3 4] `unit `never)",
        patter_std!("unit").unwrap()
    }
    eval_test_std! {
        kleene_with_pat,
        "(with? [(many (~ :foo :bar))] [:foo :bar :foo :foo :bar :bar :foo] `unit `never)",
        patter_std!("unit").unwrap()
    }
    eval_test_std! {
        consec,
        "(with? [(consec :foo :bar)] [:foo :bar] `unit `never)",
        patter_std!("unit").unwrap()
    }

    #[test]
    fn match_ptn_bindings() {
        assert_eq!(
            patter!("(a 1)").match_ptn(&patter!("(b 2)")).unwrap(),
            None
        );
    }

    #[test]
    fn convert_bindings() {
        assert_eq!(
            Bindings::from_sexpr(patter!("((:foo 4))")).unwrap(),
            Bindings::of(ident!("foo"), &SExpr::Int(4)),
        )
    }

    #[test]
    fn context() {
        let _ = Context::new();
    }
}
