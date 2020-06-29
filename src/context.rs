use std::collections::HashMap;
use std::convert::TryInto;
use std::iter::Extend;

use crate::intern::Interned;
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
pub struct Bindings(HashMap<Interned<'static, Ident>, SExpr>);

impl Bindings {
    pub fn join(mut self, other: &Bindings) -> Bindings {
        self.0
            .extend(other.0.iter().map(|(i, e)| (i.clone(), e.clone())));
        self
    }

    pub fn insert(&mut self, other: Bindings) {
        self.0.extend(other.0.into_iter());
    }

    pub fn empty() -> Bindings {
        Bindings(HashMap::new())
    }

    pub fn basic() -> Bindings {
        Bindings::empty()
            .join(&Bindings::of(
                ident!("#/sigil/tick"),
                &SExpr::Fun(
                    Box::new(SExpr::Operation(|cxt: &mut Context| {
                        get!("q-expr", cxt).quote(cxt)
                    })),
                    Box::new(SExpr::List(vec![SExpr::Place(ident!("q-expr"))])),
                    Box::new(Bindings::empty()),
                ),
            ))
            .join(&Bindings::of(
                ident!("#/sigil/comma"),
                &SExpr::Fun(
                    Box::new(SExpr::Operation(|cxt: &mut Context| {
                        SExpr::Place(get!("ptn-ident", cxt).as_ident().unwrap())
                    })),
                    Box::new(SExpr::List(vec![SExpr::Place(ident!("ptn-ident"))])),
                    Box::new(Bindings::empty()),
                ),
            ))
            .join(&Bindings::of(
                ident!("#/sigil/bracket"),
                &SExpr::Fun(
                    Box::new(SExpr::Operation(|mut cxt: &mut Context| {
                        SExpr::List(
                            get!("brk-list", cxt)
                                .as_list()
                                .unwrap()
                                .iter()
                                .map(|e| e.eval(&mut cxt, true))
                                .collect::<Vec<_>>(),
                        )
                    })),
                    Box::new(SExpr::List(vec![SExpr::Place(ident!("brk-list"))])),
                    Box::new(Bindings::empty()),
                ),
            ))
    }

    pub fn new() -> Bindings {
        Bindings::basic()
            .join(primitive!(
                "#/keyword/make",
                "[,ident]",
                Keyword(get!("ident", cxt).as_ident().expect(&format!(
                    "Keywords can only be created with identifiers, not {:?}",
                    get!("ident", cxt)
                ))),
                cxt
            ))
            .join(primitive!(
                "#/add",
                "[,lhs ,rhs]",
                Int(get!("lhs", cxt).as_int().unwrap()
                    + get!("rhs", cxt)
                        .as_int()
                        .expect(&format!("Can only add ints, not {:?}", get!("rhs", cxt)))),
                cxt
            ))
            .join(primitive!(
                "#/eq", //this will probably be not a primitive later
                "[,lhs ,rhs]",
                {
                    if get!("lhs", cxt) == get!("rhs", cxt) {
                        Keyword(ident!("true"))
                    } else {
                        Keyword(ident!("false"))
                    }
                },
                cxt
            ))
            .join(primitive!(
                "#/with?",
                "[,ptn ,expr ,consec ,alt ,scope-depth]",
                {
                    if let Some(bindings) = get!("ptn", cxt).match_ptn(&get!("expr", cxt)) {
                        let depth = get!("scope-depth", cxt)
                            .as_int()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        cxt.add_bindings_at_depth(&bindings, depth);
                        get!("consec", cxt).eval(&mut cxt, false)
                    } else {
                        get!("alt", cxt).eval(&mut cxt, false)
                    }
                },
                cxt
            ))
            .join(primitive!(
                "#/fun/make",
                "[,fun-expr ,args-ptn]",
                Fun(
                    Box::new(get!("fun-expr", defn_cxt)),
                    Box::new(get!("args-ptn", defn_cxt)),
                    Box::new(
                        defn_cxt.collapse_keeping_sorted(
                            get!("fun-expr", defn_cxt).referenced_idents()
                        )
                    ),
                ),
                defn_cxt
            ))
            .join(&Bindings::of(
                ident!("#/never"),
                &SExpr::Operation(|_: &mut Context| {
                    panic!("reached the unreachable");
                }),
            ))
            .join(primitive!(
                "#/ptn/union/make",
                "[,a ,b]",
                PtnUnion(Box::new(get!("a", cxt)), Box::new(get!("b", cxt))),
                cxt
            ))
            .join(primitive!(
                "#/ptn/intersection/make",
                "[,a ,b]",
                PtnIntersect(Box::new(get!("a", cxt)), Box::new(get!("b", cxt))),
                cxt
            ))
            .join(primitive!(
                "#/spread/make",
                "[,spread-list]",
                Spread(
                    get!("spread-list", cxt)
                        .eval(&mut cxt, false)
                        .as_list()
                        .expect(&format!(
                            "#/spread/make can only be called with a list, not {:?}",
                            get!("spread-list", cxt)
                        ))
                ),
                cxt
            ))
            .join(primitive!(
                "#/list/head",
                "[,head-list]",
                if get!("head-list", cxt).as_list().unwrap().len() < 1 {
                    List(vec![])
                } else {
                    get!("head-list", cxt).as_list().unwrap()[0].clone()
                },
                cxt
            ))
            .join(primitive!(
                "#/list/tail",
                "[,tail-list]",
                if !get!("tail-list", cxt).as_list().unwrap().is_empty() {
                    List(get!("tail-list", cxt).as_list().unwrap()[1..].to_vec())
                } else {
                    List(vec![])
                },
                cxt
            ))
    }

    pub fn of(ident: Interned<'static, Ident>, value: &SExpr) -> Bindings {
        Bindings({
            let mut m = HashMap::new();
            m.insert(ident.clone(), value.clone());
            m
        })
    }
}

impl Context {
    pub fn lookup(&self, ident: Interned<'static, Ident>) -> Option<SExpr> {
        for cxti in self.contexts.iter().rev() {
            if let Some(i) = cxti.bindings.0.get(&ident).cloned() {
                return Some(i);
            }
        }
        None
    }

    pub fn empty() -> Context {
        Context {
            contexts: vec![ContextInner {
                bindings: Bindings::empty(),
            }],
        }
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

    pub fn add_bindings(&mut self, bindings: &Bindings) {
        self.add_bindings_at_depth(bindings, 0);
    }

    pub fn add_bindings_at_depth(&mut self, bindings: &Bindings, depth: usize) {
        assert!(
            self.contexts.len() >= 1 + depth,
            "Tried to add bindings too deep"
        );
        let idx = self.contexts.len() - 1 - depth;
        self.contexts[idx].bindings.insert(bindings.clone());
    }

    pub fn push_scope(&mut self) {
        self.contexts.push(ContextInner {
            bindings: Bindings::empty(),
        });
    }

    pub fn pop_scope(&mut self) {
        self.contexts.pop();
    }

    pub fn push_context(&mut self, context: Context) {
        for inner in context.contexts {
            self.add_bindings(&inner.bindings)
        }
    }

    pub fn collapse(&self) -> Bindings {
        let mut collapsed = Bindings::empty();
        for ContextInner { bindings } in &self.contexts {
            for (id, val) in &bindings.0 {
                collapsed.0.insert(*id, val.clone());
            }
        }
        collapsed
    }

    pub fn collapse_keeping_sorted(&self, keep: Vec<Interned<'static, Ident>>) -> Bindings {
        let mut collapsed = Bindings::empty();
        for ContextInner { bindings } in &self.contexts {
            for (id, val) in &bindings.0 {
                if keep.binary_search(&id).is_ok() {
                    collapsed.0.insert(*id, val.clone());
                }
            }
        }
        collapsed
    }
}
