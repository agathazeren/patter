use std::collections::HashMap;
use std::iter::Extend;

use crate::parse;
use crate::Ident;
use crate::SExpr;

#[derive(Clone, Debug)]
pub struct Context {
    pub bindings: Bindings,
}

#[derive(Clone, Debug)]
pub struct Bindings(HashMap<Ident, SExpr>);

impl Bindings {
    pub fn join(mut self, other: Bindings) -> Bindings {
        self.0.extend(other.0.into_iter());
        self
    }

    pub fn empty() -> Bindings {
        Bindings(HashMap::new())
    }

    pub fn basic() -> Bindings {
        Bindings(HashMap::new())
            .join(Bindings::of(
                &ident!("#/sigil/tick"),
                &SExpr::NonEvalingFun(
                    Box::new(SExpr::Operation(|cxt: &mut Context| {
                        get!("q-expr", cxt).quote(cxt)
                    })),
                    Box::new(SExpr::List(vec![SExpr::Place(ident!("q-expr"))])),
                ),
            ))
            .join(Bindings::of(
                &ident!("#/sigil/comma"),
                &SExpr::NonEvalingFun(
                    Box::new(SExpr::Operation(|cxt: &mut Context| {
                        SExpr::Place(get!("ptn-ident", cxt).as_ident().unwrap())
                    })),
                    Box::new(SExpr::List(vec![SExpr::Place(ident!("ptn-ident"))])),
                ),
            ))
    }

    pub fn new() -> Bindings {
        Bindings::basic()
            .join(primitive!(
                "#/add",
                "`(,lhs ,rhs)",
                Int(
                    get!("lhs", cxt).as_int().unwrap()
                        + get!("rhs", cxt).as_int().unwrap()
                ),
                cxt
            ))
            .join(primitive!(
                "#/bind-expr",
                "`(,ident ,expr)",
                {
                    cxt.bindings
                        .0
                        .insert(get!("ident", cxt).as_ident().unwrap(), get!("expr", cxt));
                    List(vec![])
                },
                cxt
            ))
            .join(primitive!(
                "#/do",
                ",exprs",
                {
                    match get!("exprs", cxt) {
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
                "`(,exprs)",
                {
                    let mut results = Vec::new();
                    match get!("exprs", cxt) {
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
                "#/do/ret-last",
                "`(,exprs)",
                {
                    let mut result = List(vec![]);
                    match get!("exprs", cxt) {
                        List(exprs) => {
                            for expr in exprs {
                                result = expr.eval(&mut cxt);
                            }
                        }
                        _ => unreachable!(),
                    }
                    result
                },
                cxt
            ))
            .join(primitive!(
                "#/eq", //this will probably be not a primitive later
                "`(,lhs ,rhs)",
                {
                    if get!("lhs", cxt) == get!("rhs", cxt) {
                        Keyword(crate::Ident::Name("true".to_string()))
                    } else {
                        Keyword(crate::Ident::Name("false".to_string()))
                    }
                },
                cxt
            ))
            .join(primitive!(
                "#/with?",
                "`(,ptn ,expr ,consec ,alt)",
                {
                    if let Some(bindings) = get!("expr", cxt).match_ptn(&get!("ptn", cxt)) {
                        *cxt = Context {
                            bindings: cxt.bindings.clone().join(bindings),
                            ..*cxt
                        };
                        get!("consec", cxt).eval(&mut cxt)
                    } else {
                        get!("alt", cxt).eval(&mut cxt)
                    }
                },
                cxt
            ))
            .join(primitive_noeval!(
                "#/fun/make",
                "`(,fun-expr ,args-ptn)",
                {
                    Fun(
                        Box::new(get!("fun-expr", cxt)),
                        Box::new(get!("args-ptn", cxt).eval(&mut cxt)),
                    )
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
    }}

impl Context {
    pub fn lookup(&self, ident: &Ident) -> Option<SExpr> {
        self.bindings.0.get(&ident).cloned()
    }

    pub fn basic() -> Context {
        Context {
            bindings: Bindings::basic(),
        }
    }

    pub fn new() -> Context {
        Context {
            bindings: Bindings::new(),
        }
    }
}
