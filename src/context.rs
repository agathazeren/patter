use std::collections::HashMap;
use std::iter::Extend;

use crate::parse;
use crate::Ident;
use crate::SExpr;

#[derive(Clone, Debug)]
pub struct Context {
    contexts: Vec<ContextInner>,
}

#[derive(Clone, Debug)]
struct ContextInner {
    pub bindings: Bindings,
}

#[derive(Clone, Debug)]
pub struct Bindings(HashMap<Ident, SExpr>);

impl Bindings {
    pub fn join(mut self, other: Bindings) -> Bindings {
        self.0.extend(other.0.into_iter());
        self
    }

    pub fn insert(&mut self, other: Bindings) {
        self.0.extend(other.0.into_iter());
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
                Int(dbg!(
                    dbg!(get!("lhs", cxt).as_int().unwrap())
                        + dbg!(get!("rhs", cxt).as_int().unwrap())
                )),
                cxt
            ))
            .join(primitive!(
                "#/bind-expr",
                "`(,ident ,expr)",
                {
                    cxt.add_super_bindings(Bindings::of(
                        &get!("ident", cxt).as_ident().unwrap(),
                        &get!("expr", cxt),
                    ));
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
            .join(primitive_noeval!(
                "#/with?",
                "`(,ptn ,expr ,consec ,alt)",
                {
                    dbg!();
                    if let Some(bindings) = dbg!(get!("ptn", cxt).eval(&mut cxt))
                        .match_ptn(&dbg!(get!("expr", cxt).eval(&mut cxt)))
                    {
                        cxt.add_bindings(bindings);
                        dbg!(get!("consec", cxt).eval(&mut cxt))
                    } else {
                        dbg!(get!("alt", cxt).eval(&mut cxt))
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
    }
}

impl Context {
    pub fn lookup(&self, ident: &Ident) -> Option<SExpr> {
        for cxti in self.contexts.iter().rev() {
            if let Some(i) = cxti.bindings.0.get(&ident).cloned() {
                return Some(i);
            }
        }
        None
    }

    pub fn basic() -> Context {
        Context {
            contexts: vec![ContextInner {
                bindings: Bindings::basic(),
            }],
        }
    }

    pub fn new() -> Context {
        Context {
            contexts: vec![ContextInner {
                bindings: Bindings::new(),
            }],
        }
    }

    pub fn add_bindings(&mut self, bindings: Bindings) {
        self.contexts.last_mut().unwrap().bindings.insert(bindings);
    }

    pub fn add_super_bindings(&mut self, bindings: Bindings) {
        assert!(self.contexts.len() >= 2);
        let idx = self.contexts.len() - 2;
        self.contexts[idx].bindings.insert(bindings);
    }

    pub fn push_scope(&mut self) {
        self.contexts.push(ContextInner{
            bindings: Bindings::empty(),
        });
    }

    pub fn pop_scope(&mut self) {
        self.contexts.pop();
    }
}
