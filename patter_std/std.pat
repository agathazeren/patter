(#/defnever `never)

(#/with?
    ,#/sigil/colon
    (#/fun/make
        `(#/unary-sigil-app/make `: it)
        [,it]
    )
    [`: (#/unary-sigil-app/make `` `())]
    `never
    4
)

(#/with? ,unit :() :() ``never 4)

(#/with?
    ,def
    (#/fun/make
        `[
            (#/with? name value `unit `never 8)
        ]
        [,name ,value]
    )
    `unit
    `never
    4
)

(def ,dbg #/dbg)

(def ,#/sigil/amp #/spread/make)

(def ,list/head #/list/head)
(def ,list/tail #/list/tail)

(def ,ident/from-components #/ident/from-components)
(def ,ident/from-int #/ident/from-int)
(def ,ident/concat #/ident/concat)

(def ,at-ptn-time #/ptn/at-ptn-time/make)

(def ,with? (#/fun/make
    `(#/with? ptn expr conseq alter 0)
    [,ptn ,expr ,conseq ,alter]
))

(def ,with (#/fun/make
    `(with? ptn expr conseq `never)
    [,ptn ,expr ,conseq]
))

; DEPRECATED
(def ,if (#/fun/make
    `(with? :true cond consec alt)
    [,cond ,consec ,alt]
))

(def ,any ,#/noread)

(def ,list/contains (#/fun/make
    `(with? target (list/head list)
        `:true
        `(with? [] (list/tail list)
            :false
            `(list/contains (list/tail list) target)
        )
    )
    [,list ,target]
))

(def ,list/dedup' (#/fun/make
    `(with? [] in
        `out
        `(if (list/contains out (list/head in))
            `(list/dedup' out (list/tail in))
            `(list/dedup' [&out (list/head in)] (list/tail in))
        )
     )
     [,out ,in]
))

(def ,list/dedup (#/fun/make
    `(list/dedup' [] list)
    [,list]
))

(def ,bindings any) ; list of pairs of ident -> expr

(def ,bindings/join (#/fun/make
    `(list/dedup [&a &b])
    [,a ,b] ; this is used in ^, so we can't constrain these parameters further
))

(def ,ptn/union/acc [
    (#/fun/make
        `(with? [[:some bindings] [:some bindings]] [a b]
            `[:some (bindings/join (list/head (list/tail a)) (list/head (list/tail b)))]
            `(with? [[:some bindings] any] [a b]
                `[:some (list/head (list/tail a))]
                `(with? [any [:some bindings]] [a b]
                     `[:some (list/head (list/tail b))]
                     `[:none]
                )
            )
        )
        [,a ,b]
    )
    [:none]
])

(def ,ptn/union/make (#/fun/make
    `(#/ptn/acc/make ptn/union/acc args)
    ,args
))

(def ,#/sigil/tilde ptn/union/make)
(def ,#/sigil/plus ptn/union/make)

(def ,ptn/intersect/acc [
    (#/fun/make
        `(with? [[:some bindings] [:some bindings]] [a b]
            `[:some (bindings/join (list/head (list/tail a)) (list/head (list/tail b)))]
            `[:none]
        )
        [,a ,b]
    )
    [:some []]
])

(def ,ptn/intersect/make (#/fun/make
    `(#/ptn/acc/make ptn/intersect/acc args)
    ,args
))

(def ,#/sigil/carrot ptn/intersect/make)

(def ,many (#/fun/make
    `(#/kleene/make [] (#/fun/make
        `pat
        [any]
    ))
    [,pat]
))

(def ,consec (#/fun/make
    `(#/ptn/consec/make args)
    ,args
))

(def ,succ (#/fun/make
    `(#/add 1 n)
    [,n]
))


(def ,list/len (#/fun/make
    `(with? [] it
        `0
        `(succ (list/len (list/tail it)))
    )
    [,it]
))

(def ,list/map (#/fun/make
    `(with? [] map-list
        `[]
        `[
            (trans (list/head map-list))
            &(list/map trans (list/tail map-list))
        ]
    )
    [,trans ,map-list]
))


(def ,default-args [(#/kleene/make
    []
    (#/fun/make
        `(, (#/ident/concat `' (#/ident/from-int (list/len prev))))
        [,prev]
    )
)])


(def ,melt (#/fun/make
    `ti
    [(: ,ti)]
))

(def ,std-is-here 42)

(def ,id (#/fun/make
    `it
    [,it]
))

(def ,bind (#/fun/make
    `(#/ptn/acc/make
        [
            (#/fun/make
                `(with? [[:some ,a] [:some ,b]] [a b]
                    `[:some (bindings/join (bindings/join a b) [[(: name) val]])]
                    `[:none]
                )
                [,a ,b]
            )
            [:some []]
        ]
        [
            (consec)
        ]
    )
    [,name ,val]
))


(def ,arg? (#/fun/make
    `(~ (, name) (bind name val))
    [,name ,val]
))

(def ,#/sigil/backslash (#/fun/make
     `(#/fun/make body args-pat)
     [(arg? `args-pat default-args) ,body]
))

(def ,fib (\ [,n]
    `(with? 0 n
        `0
        `(with? 1 n
            `1
            `(#/add
                (fib (#/add n -1))
                (fib (#/add n -2))
            )
        )
    ) 
))


(def ,vow (\ [,thing]
    `(#/zero-width thing)
))