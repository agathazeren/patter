(#/bind-expr `std-is-here 42)

(#/bind-expr `with? #/with?)

(#/bind-expr `fib (#/fun/make
    (with? 0 n
        0
        (with? 0 1
            1
            (#/add
                (fib (#/add n -1))
                (fib (#/add n -2))
            )
        )
    ) 
    `(,n)
))