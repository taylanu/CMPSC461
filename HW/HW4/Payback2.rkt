
;; "Some credit-card companies pay back a small portion of the
;;  charges a customer makes over a year. One company returns:"
#|
a) 0.5% for the first $1000 (between $0-$1000)
b) 0.75% for the next $1000 (between $1000-$2000)
c) 1.0% for the next $1500 (between $2000-$3500)
d) and 1.5% for everything else above $3500
;;do the other pluses, because only one of these cases are hit, so add the cashback from the rest of the lower functions.
|#


;; CMPSC461 HW4 Prob4 Taylan Unal (tuu2)
;; Payback Function
(define (payback chg) ;;function consumes charge amount, calculates pay amount.
  ;;(let ((pay 0)) ;;define local variable for pay. No need
    (cond
      ((> chg 3500) (+ (* (- chg 3500) 0.015) 15 7.5 5)) ;; add the cashback from lower value functions. Cashback values are known for lower charge amounts
      ((> chg 2000) (+ (* (- chg 2000) 0.01) 7.5 5))
      ((> chg 1000) (+ (* (- chg 1000) 0.0075) 5))
      ((>= chg 0) (* chg 0.005))
      ( else 'ERROR );;Throw error, potentially if somehow negative charge is passed.
    )
)

;; THIS NEEDS TO BE IF STRUCTURED. Right now its just a set of ifs. SAVING AS DRAFT

#|
TESTBENCH:
Input: $400 charged/yr Output: $2 paid back [0.5% of $400=$2] [VERIFIED]
Input: $1400 charged/yr Output: $8 paid back (0.5% of $1000=$5 and 0.75% of $400=$3 [VERIFIED]
Input: $4000 charged. Output: Should be $35. (1.5% of $500=$7.5, + next $1500*0.01 = $15 + next $1000*0.0075=$7.5, + next $1000*0.005=5
|#


