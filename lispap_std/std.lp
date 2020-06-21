(#/with? ,unit (#/keyword/make `unit) (#/keyword/make `unit) #/never 1)
(#/with? ,never #/never unit #/never 1)

(#/with? ,def (#/fun/make
    `[
        (#/with? name value unit never 3)
    ]
    [,name ,value]
) unit never 1)

(def ,#/sigil/colon #/keyword/make)
(def ,#/sigil/tilde #/ptn/union/make)
(def ,#/sigil/carrot #/ptn/intersection/make)


(def ,std-is-here 42)

(def ,id (#/fun/make
    `it
    [,it]
))

(def ,with? (#/fun/make
    `(#/with? ptn expr conseq alter 0)
    [,ptn ,expr ,conseq ,alter]
))

(def ,fib (#/fun/make
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
