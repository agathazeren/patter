use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::fmt::Display;
use std::iter::Extend;

use crate::intern::Interned;
use crate::parse;
use crate::Ident;
use crate::{IntoSExpr, FromSExpr};
use crate::SExpr;
use crate::IDENTS;
use crate::LISPAP_STD_STR;

#[derive(Clone, Debug)]
pub struct Context {
    contexts: Vec<ContextInner>,
}

#[derive(Clone, Debug)]
struct ContextInner {
    pub bindings: Bindings,
}

#[derive(Clone, Debug, PartialEq)]
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
                &SExpr::Fun(crate::Fun {
                    body: Box::new(SExpr::Operation {
                        eval: |cxt: &mut Context| Ok(get!("q-expr", cxt)),
                        evals_to:
                            |cxt: &dyn Fn(
                                Interned<'static, Ident>,
                            )
                                -> Option<SExpr>| {
                                cxt(ident!("q-expr")).unwrap()
                            },
                    }),
                    args_ptn: Box::new(SExpr::List(vec![SExpr::Place(
                        ident!("q-expr"),
                    )])),
                    closure: Box::new(Bindings::empty()),
                }),
            ))
            .join(&Bindings::of(
                ident!("#/sigil/comma"),
                &SExpr::Fun(crate::Fun {
                    body: Box::new(SExpr::Operation {
                        eval: |cxt: &mut Context| {
                            Ok(SExpr::Place(
                                get!("ptn-ident", cxt).as_ident().unwrap(),
                            ))
                        },
                        evals_to:
                            |cxt: &dyn Fn(
                                Interned<'static, Ident>,
                            )
                                -> Option<SExpr>| {
                                SExpr::LitMatch(Box::new(SExpr::Place(
                                    cxt(ident!("ptn-ident"))
                                        .unwrap()
                                        .as_ident()
                                        .unwrap(),
                                )))
                            },
                    }),
                    args_ptn: Box::new(SExpr::List(vec![SExpr::Place(
                        ident!("ptn-ident"),
                    )])),
                    closure: Box::new(Bindings::empty()),
                }),
            ))
            .join(&Bindings::of(
                ident!("#/sigil/bracket"),
                &SExpr::Fun(crate::Fun {
                    body: Box::new(SExpr::Operation {
                        eval: |mut cxt: &mut Context| {
                            Ok(SExpr::List(
                                get!("brk-list", cxt)
                                    .as_list()
                                    .unwrap()
                                    .iter()
                                    .map(|e| e.eval(&mut cxt))
                                    .collect::<Result<_, _>>()?,
                            ))
                        },
                        evals_to:
                            |cxt: &dyn Fn(
                                Interned<'static, Ident>,
                            )
                                -> Option<SExpr>| {
                                SExpr::List(
                                    cxt(ident!("brk-list"))
                                        .unwrap()
                                        .as_list()
                                        .unwrap()
                                        .iter()
                                        .map(|e| e.clone().as_ident().unwrap())
                                        .map(cxt)
                                        .collect::<Option<_>>()
                                        .unwrap(),
                                )
                            },
                    }),
                    args_ptn: Box::new(SExpr::List(vec![SExpr::Place(
                        ident!("brk-list"),
                    )])),
                    closure: Box::new(Bindings::empty()),
                }),
            ))
    }

    pub fn new() -> Bindings {
        Bindings::basic()
            .join(primitive!(
                "#/add",
                "[,lhs ,rhs]",
                Int(get!("lhs", cxt)
                    .as_int()
                    .expect(&format!("Can only add ints, not {:?}", get!("lhs", cxt)))
                    + get!("rhs", cxt)
                        .as_int()
                    .expect(&format!("Can only add ints, not {:?}", get!("rhs", cxt)))),
                Place(ident!("#/noread")),
                cxt
            ))
            .join(primitive!(
                "#/with?",
                "[,ptn ,expr ,consec ,alt ,scope-depth]",
               {
                    if let Some(bindings) = get!("ptn", cxt).match_ptn(&get!("expr", cxt))? {
                        let depth = get!("scope-depth", cxt)
                            .as_int()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        cxt.add_bindings_at_depth(&bindings, depth);
                        get!("consec", cxt).eval(&mut cxt)?
                    } else {
                        get!("alt", cxt).eval(&mut cxt)?
                    }
                },
                //TODO Make this better
                lispap_sr!(
                    cxt(ident!("ptn/union/make")).unwrap().as_fun().unwrap(), List(vec![
                        cxt(ident!("consec")).unwrap().evals_to(),
                        cxt(ident!("alt")).unwrap().evals_to()
                    ])
                ).unwrap(),
                cxt
            ))
            .join(primitive!(
                "#/fun/make",
                "[,fun-expr ,args-ptn]",
                Fun(crate::Fun{
                    body: Box::new(get!("fun-expr", defn_cxt)),
                    args_ptn: Box::new(get!("args-ptn", defn_cxt)),
                    closure: Box::new(
                        defn_cxt.collapse_keeping_sorted(
                            get!("fun-expr", defn_cxt).referenced_idents()
                        )
                    ),
                }),
                //TODO Make this better
                Place(ident!("#/noread")),
                defn_cxt
            ))
            .join(&Bindings::of(
                ident!("#/never"),
                &SExpr::Operation{
                    eval: |_: &mut Context| {
                        throw_interpreter_err!(ReachedTheUnreachable);
                        unreachable!();
                    },
                    evals_to: |cxt: &dyn Fn(Interned<'static, Ident>) -> Option<SExpr>| {
                        lispap_sr!(
                            cxt(ident!("ptn/union/make")).unwrap().as_fun().unwrap(),
                            SExpr::List(Vec::new())
                        ).unwrap()
                    }
                },
            ))
            .join(primitive!(
                "#/spread/make",
                "[,spread-list]",
                Spread(
                    get!("spread-list", cxt)
                        .eval(&mut cxt)?
                        .as_list()
                        .expect(&format!(
                            "#/spread/make can only be called with a list, not {:?}",
                            get!("spread-list", cxt)
                        ))
                ),
                Spread(
                    cxt(ident!("spread-list"))
                        .unwrap()
                        .as_list()
                        .unwrap()
                        .iter()
                        .map(|e| e.evals_to())
                        .collect::<Vec<_>>()
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
                //TODO
                Place(ident!("#/noread")),
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
                //TODO
                Place(ident!("#/noread")),
                cxt
            ))
            .join(primitive!(
                "#/unary-sigil-app/make",
                "[,sigil ,expr]",
                UnarySigilApp(
                    get!("sigil", cxt).as_sigil().unwrap(),
                    Box::new(get!("expr", cxt))
                ),
                //TODO
                Place(ident!("#/noread")),
                cxt
            ))
            .join(primitive!(
                "#/ident/from-components",
                "[,components]",
                Ident(IDENTS.intern(crate::Ident{
                    names: get!("components", cxt)
                        .as_list()
                        .unwrap()
                        .iter()
                        .map(|e| e.clone().as_ident().unwrap().names[0].clone())
                        .collect::<Vec<_>>(),
                    tl_ns: false
                })),
                //TODO
                Place(ident!("#/noread")),
                cxt
            ))
            .join(primitive!(
                "#/ident/from-int",
                "[,n]",
                Ident(IDENTS.intern(crate::Ident{
                    names: vec![get!("n", cxt).as_int().unwrap().to_string()],
                    tl_ns: false
                })),
                //TODO
                Place(ident!("#/noread")),
                cxt
            ))
            .join(primitive!(
                "#/ptn/rest/make",
                "[,pat]",
                Rest(Box::new(get!("pat", cxt))),
                //TODO
                Place(ident!("#/noread")),
                cxt
            ))
            .join(primitive!(
                "#/ptn/at-ptn-time/make",
                "[,fun]",
                AtPtnTime(Box::new(get!("fun", cxt))),
                //TODO
                Place(ident!("#/noread")),
                cxt
            ))
            .join(primitive!(
                "#/ident/concat",
                "[,a ,b]",
                {
                    let a = get!("a", cxt).as_ident().unwrap();
                    let b = get!("b", cxt).as_ident().unwrap();
                    assert!(a.names.len() == 1 && b.names.len() == 1 && !a.tl_ns && !a.tl_ns);
                    Ident(IDENTS.intern(crate::Ident{
                        names: vec![a.names[0].clone() + &b.names[0].clone()],
                        tl_ns: false,
                    }))
                },
                //TODO
                Place(ident!("#/noread")),
                cxt
            ))
            .join(primitive!(
                "#/ptn/acc/make",
                "[[,acc ,init] ,pats]",
                PtnAcc{
                    acc: get!("acc", cxt).as_fun().unwrap(),
                    init: Option::<Bindings>::from_sexpr(get!("init", cxt))?,
                    pats: get!("pats", cxt).as_list().unwrap(),
                },
                LitMatch(Box::new(PtnAcc{
                    acc:                     cxt(ident!("acc")).unwrap().as_fun().unwrap(),
                    init: Option::<Bindings>::from_sexpr(cxt(ident!("init")).unwrap()).unwrap(),
                    pats: cxt(ident!("pats")).unwrap().as_list().unwrap()
                })),
                cxt
            ))
            .join(primitive!(
                "#/dbg",
                "[,it]",
                {
                    print!("{:#?}", get!("it", cxt));
                    get!("it", cxt)
                },
                UnarySigilApp('`',Box::new(cxt(ident!("it")).unwrap())),
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

    pub fn of_contents(
        map: HashMap<Interned<'static, Ident>, SExpr>,
    ) -> Bindings {
        Bindings(map)
    }

    pub fn referenced_idents_sorted(&self) -> Vec<Interned<'static, Ident>> {
        let mut idents = Vec::new();
        for (ident, expr) in &self.0{
            idents.push(*ident);
            idents.extend(expr.referenced_idents());
        }
        idents.sort();
        idents
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

    pub fn std() -> Context {
        let mut cxt = Context::new();
        lispap!(&format!("[{}]", *LISPAP_STD_STR))
            .eval(&mut cxt)
            .unwrap();
        cxt
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

    pub fn collapse_keeping_sorted(
        &self,
        keep: Vec<Interned<'static, Ident>>,
    ) -> Bindings {
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

impl Display for Bindings {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<BINDINGS>")
    }
}

impl IntoSExpr for Bindings {
    fn into_sexpr(self) -> SExpr {
        SExpr::List(
            self.0
                .iter()
                .map(|(i, b)| SExpr::List(vec![SExpr::Ident(*i), b.clone()]))
                .collect(),
        )
    }
}
