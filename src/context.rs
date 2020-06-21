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
                &SExpr::Fun(
                    Box::new(SExpr::Operation(|cxt: &mut Context| {
                        get!("q-expr", cxt).quote(cxt)
                    })),
                    Box::new(SExpr::Place(ident!("q-expr"))),
                ),
            ))
            .join(Bindings::of(
                &ident!("#/sigil/comma"),
                &SExpr::Fun(
                    Box::new(SExpr::Operation(|cxt: &mut Context| {
                        SExpr::Place(get!("ptn-ident", cxt).as_ident().unwrap())
                    })),
                    Box::new(SExpr::Place(ident!("ptn-ident"))),
                ),
            ))
            .join(Bindings::of(
                &ident!("#/sigil/bracket"),
                &SExpr::Fun(
                    Box::new(SExpr::Operation(|mut cxt: &mut Context| {
                        SExpr::List(
                            get!("list", cxt)
                                .as_list()
                                .unwrap()
                                .iter()
                                .map(|e| e.eval(&mut cxt, false))
                                .collect::<Vec<_>>(),
                        )
                    })),
                    Box::new(SExpr::Place(ident!("list"))),
                ),
            ))
    }

    pub fn new() -> Bindings {
        Bindings::basic()
            .join(primitive!(
                "#/keyword/make",
                ",ident",
                Keyword(get!("ident", cxt).as_ident().expect(&format!(
                    "Keywords can only be created with identifiers, not {:?}",
                    get!("ident", cxt)
                ))),
                cxt
            ))
            .join(primitive!(
                "#/add",
                "[,lhs ,rhs]",
                Int(dbg!(
                    dbg!(get!("lhs", cxt).as_int().unwrap())
                        + dbg!(get!("rhs", cxt).as_int().unwrap())
                )),
                cxt
            ))
            .join(primitive!(
                "#/bind-expr",
                "[,ident ,expr]",
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
                "#/eq", //this will probably be not a primitive later
                "[,lhs ,rhs]",
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
                "[,ptn ,expr ,consec ,alt ,scope]",
                {
                    if let Some(bindings) = dbg!(get!("ptn", cxt).eval(&mut cxt, false))
                        .match_ptn(&dbg!(get!("expr", cxt).eval(&mut cxt, false)))
                    {
                        match get!("scope", cxt).eval(&mut cxt, false) {
                            e if e == Keyword(ident!("scoped")) => cxt.add_bindings(bindings),
                            e if e == Keyword(ident!("super")) => cxt.add_super_bindings(bindings),
                            e => panic!("Bad scope argument to with: {:?}", e),
                        }
                        dbg!(get!("consec", cxt).eval(&mut cxt, false))
                    } else {
                        dbg!(get!("alt", cxt).eval(&mut cxt, false))
                    }
                },
                cxt
            ))
            .join(primitive!(
                "#/fun/make",
                "[,fun-expr ,args-ptn]",
                {
                    Fun(
                        Box::new(get!("fun-expr", cxt)),
                        Box::new(get!("args-ptn", cxt)),
                    )
                },
                cxt
            ))
            .join(primitive!(
                "#/never",
                "`()",
                panic!("reached an unreachable"),
                cxt
            ))
        /*            .join(primitive!(
            "#/list",
            "(#/sigil/tick ,args)",
            {
                List(
                    get!("args", cxt)
                        .as_list()
                        .unwrap()
                        .iter()
                        .map(|e| dbg!(e.eval(&mut cxt, false)))
                        .collect::<Vec<_>>(),
                )
            },
            cxt
        ))*/
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
        self.contexts.push(ContextInner {
            bindings: Bindings::empty(),
        });
    }

    pub fn pop_scope(&mut self) {
        self.contexts.pop();
    }
}
