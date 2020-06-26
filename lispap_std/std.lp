(#/with? ,unit (#/keyword/make `unit) (#/keyword/make `unit) #/never 2)
(#/with? ,never #/never unit #/never 2)

(#/with? ,def (#/fun/make
    `[
        (#/with? name value unit never 8)
    ]
    [,name ,value]
) unit never 2)

(def ,#/sigil/colon #/keyword/make)
(def ,#/sigil/tilde #/ptn/union/make)
(def ,#/sigil/carrot #/ptn/intersection/make)
(def ,#/sigil/amp #/spread/make)

; (def ,default-args (#/fun/make
;     `[
;         
;     ]
;     ,body
; ))

(def ,list/head #/list/head)

(def ,list/tail #/list/tail)


(def ,with? (#/fun/make
    `(#/with? ptn expr conseq alter 0)
    [,ptn ,expr ,conseq ,alter]
))


(def ,list/map (#/fun/make
    `(with? []  map-list
        []
        `[
            (trans (list/head map-list))
            &(list/map trans (list/tail map-list))
        ]
    )
    [,trans ,map-list]
))

(def ,std-is-here 42)

(def ,id (#/fun/make
    `it
    [,it]
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
