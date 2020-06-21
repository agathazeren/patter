(#/bind-expr `#/sigil/colon #/keyword/make
)
(#/bind-expr `std-is-here 42)

(#/bind-expr `unit :unit)

(#/bind-expr `id (#/fun/make
    `it
    [,it]
))

(#/bind-expr `with? (#/fun/make
    `(#/with? (id ptn) (id expr) conseq alter :scoped)
    [,ptn ,expr ,conseq ,alter]
))

(#/bind-expr `never #/never)

(#/bind-expr `def (#/fun/make
    `(#/with? name value unit never :super)
    [,name ,value]
))

(#/bind-expr `fib (#/fun/make
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
    [,n]
))
