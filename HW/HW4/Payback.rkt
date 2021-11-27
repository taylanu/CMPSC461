;; CMPSC461 HW4 Prob4 Taylan Unal (tuu2)
;; Payback Function
;; "Some credit-card companies pay back a small portion of the
;;  charges a customer makes over a year. One company returns:"
#|
a) 0.5% for the first $1000 (between $0-$1000)
b) 0.75% for the next $1000 (between $1000-$2000)
c) 1.0% for the next $1500 (between $2000-$3500)
d) and 1.5% for everything else above $3500
|#

(define (payback chg) ;;function consumes charge amount, calculates pay amount.
  (let ((pay 0)) ;;define local variable for pay. Working
    (if (> chg 3500)
        (+ pay (* (- chg 3500) 0.015))
        (if (> chg 2000)
            (+ pay (* (- chg 2000) 0.01))
            (if (> chg 1000)
               (+ pay (* (- chg 1000) 0.0075))
               (if (< chg 1000)
                   (+ pay (* chg 0.005))
               )
            )
         )
     )
     ;;(+ pay (* chg 0.005)) ;; else case. Hits everytime. Never processes others.
   )
)

;; THIS NEEDS TO BE IF STRUCTURED.
;; IF STRUCTURE is (if (conditional-expression) (then-expression) (else-expression))

#|
TESTBENCH:
Input: $400 charged/yr Output: $2 paid back [0.5% of $400=$2] [VERIFIED]
Input; $1400 charged/yr Output: $8 paid back (0.5% of $1000=$5 and 0.75% of $400=$3) [NOT WORKING]
|#


 