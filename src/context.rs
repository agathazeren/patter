
use std::collections::HashMap;
use std::iter::Extend;

use crate::parse;
use crate::SExpr;
use crate::Ident;

#[derive(Clone, Debug)]
pub struct Context {
    pub bindings: Bindings,
}

#[derive(Clone, Debug)]
pub struct Bindings(HashMap<Ident, SExpr>);


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
                    use crate::SExpr::*;
                    $impl
                })),
                Box::new(parse::parse($ptn)),
            ),
        )
    };
}

impl Bindings {
    pub fn join(mut self, other: Bindings) -> Bindings {
        self.0.extend(other.0.into_iter());
        self
    }

    pub fn new() -> Bindings {
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

    pub fn of(ident: &Ident, value: &SExpr) -> Bindings {
        Bindings({
            let mut m = HashMap::new();
            m.insert(ident.clone(), value.clone());
            m
        })
    }
}

impl Context {
    pub fn lookup(&self, ident: &Ident) -> Option<SExpr> {
        self.bindings.0.get(&ident).cloned()
    }

    pub fn new() -> Context {
        Context {
            bindings: Bindings::new(),
        }
    }
}

