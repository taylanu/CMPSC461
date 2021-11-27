;; CMPSC461 HW4 Prob2 Taylan Unal (tuu2)
;; Ackerman Function
(define (A m n) ;; Ackerman with 2 parameters m,n
  (if (< m 0) 'ERROR ;;no case for a negative m value
      (if(= m 0) ;; CASE1
         (+ n 1)
         (if(and (> m 0)(= n 0)) ;; CASE2
            (A (- m 1) 1)
            (if(and (> m 0)(> n 0)) ;; CASE3
               (A (- m 1)(A m (- n 1)))
               'ERROR ;; final error (if m is pos, n is negative)
             )
          )
       )
   )
)

;; CMPSC461 HW4 Prob3 Taylan Unal (tuu2)
;; McCarthy 91 Function
(define (Mac n)
  (cond ((> n 100) ;; CASE1 
         (- n 10))
        ((<= n 100) ;; CASE2
         (Mac (Mac (+ n 11))))
  )
)

;; CMPSC461 HW4 Prob4 Taylan Unal (tuu2)
;; Payback Function
(define (payback chg) ;;function consumes charge amount, calculates pay amount.
  ;;(let ((pay 0)) ;;define local variable for pay. No need
    (cond
      ((> chg 3500) (+ (* (- chg 3500) 0.015) 15 7.5 5)) ;; add the cashback from lower value functions. Cashback values are known for lower charge amounts
      ((> chg 2000) (+ (* (- chg 2000) 0.01) 7.5 5))
      ((> chg 1000) (+ (* (- chg 1000) 0.0075) 5))
      ((>= chg 0) (* chg 0.005))
      (else 'ERROR) ;;Throw error, potentially if somehow negative charge is passed.
    )
)