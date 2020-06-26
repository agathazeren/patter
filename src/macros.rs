macro_rules! lispap {
    ($code:expr) => {{
        let tokens = parse::lex($code);
        parse::parse(&tokens)
    }};
}

macro_rules! ident {
    ($ident:expr) => {{
        let tokens = parse::lex($ident);
        crate::IDENTS.intern(parse::parse_ident(&tokens))
    }};
}

macro_rules! get {
    ($ident:expr, $cxt:expr) => {
        $cxt.clone()
            .lookup(ident!($ident))
            .expect(&format!("Ident not found: {:?}", $ident))
    };
}

macro_rules! primitive {
    ($name:expr, $ptn:expr, $impl:expr, $cxt:ident) => {
        &Bindings::of(
            ident!($name),
            &SExpr::Fun(
                #[allow(unused_mut, unused_variables)]
                Box::new(SExpr::Operation(|mut $cxt: &mut Context| {
                    #[allow(unused_imports)]
                    use crate::SExpr::*;
                    $impl
                })),
                Box::new(lispap!($ptn).eval(&mut Context::basic(), false)),
                Box::new(Context::basic().collapse()),
            ),
        )
    };
}
