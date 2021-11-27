;; CMPSC461 HW4 Prob2 Taylan Unal (tuu2)
;; Ackerman Function
(define (A m n) ;;setup ackerman with 2 parameters m,n
  (cond ((equal? m 0)
         (+ n 1))
        ((equal? n 0)
         (A (- m 1) 1))
        (else (A (- m 1)(A m (- n 1))
          )
         )
   )
)

#| TESTBENCH:
Input: A(3,5) Output: 253 (VERIFIED)


|#