;;------------------------------------------------------------------------
;; #4 to-words function
;; dont use cond statement with 200 tests.
;; split up each number into ones and tens place, also have to consider negative numbers (a simple if case)
;; Scope of definitions is only from -99 to 99 inclusive.

(define (to-words n)
  ;;define words for ones places 
  (define ones '(one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen))
  (define tens '(twenty thirty fourty fifty sixty seventy eighty ninety))
  (define output '() ) ;; output list
  ;;begin logic to determine words for n
  (cond ;; START TOP COND
    ((= n 0) '(zero)) ;; simple basecase for 0. return zero.
    (else ;; START TOP ELSE
     (cond
       ((< n -99) '(ERROR: OUT OF BOUNDS)) ;;works
       ((> n 99) '(ERROR: OUT OF BOUNDS)) ;;works
       ((< -100 n 0) ;;START NEGATIVE READOUT
        (cons 'negative ;;constructs a list based on output of below.
         (cond ;;replicated code for both positive and negative. Simply have a switch?
          ((< 0 (abs n) 20) ;;ALL VALUES FROM 1-19 are in ONES LIST
           (cons (list-ref ones (- (abs n) 1)) output)) ;;removed result
          ((< 20 (abs n) 30)
           (cons 'twenty (list (list-ref ones (- (abs n) 21)))))
          ((< 30 (abs n) 40)
           (cons 'thirty (list (list-ref ones (- (abs n) 31)))))
          ((< 40 (abs n) 50)
           (cons 'fourty (list (list-ref ones (- (abs n) 41)))))
          ((< 50 (abs n) 60)
           (cons 'fifty (list (list-ref ones (- (abs n) 51)))))
          ((< 60 (abs n) 70)
           (cons 'sixty (list (list-ref ones (- (abs n) 61)))))
          ((< 70 (abs n) 80)
           (cons 'seventy (list (list-ref ones (- (abs n) 71)))))
          ((< 80 (abs n) 90)
           (cons 'eighty (list (list-ref ones (- (abs n) 81)))))
          ((< 90 (abs n) 100)
           (cons 'ninety (list (list-ref ones (- (abs n) 91)))))
          ((integer? (quotient (abs n) 10)) ;;used for clean tens values without ones
           (list (list-ref tens (- (quotient (abs n) 10) 2))))
          (else '(ERROR: CANT RETURN RESULT))) ;; if no case found for n. UNLIKELY
          ))
       ;;END NEGATIVE READOUT
       
       ;; START POSITIVE READOUT
       ((= n 0) result) ;;possibly replicated code with line 69
       ((< 0 n 20) ;;ALL VALUES FROM 1-19 are in ONES LIST
        (cons (list-ref ones (- n 1)) output))
       ((< 20 n 30)
        (cons 'twenty (list (list-ref ones (- n 21)))))
       ((< 40 n 50)
        (cons 'fourty (list (list-ref ones (- n 41)))))
       ((< 50 n 60)
        (cons 'fifty (list (list-ref ones (- n 51)))))
       ((< 60 (abs n) 70)
        (cons 'sixty (list (list-ref ones (- n 61)))))
       ((< 70 (abs n) 80)
        (cons 'seventy (list (list-ref ones (- n 71)))))
       ((< 80 (abs n) 90)
        (cons 'eighty (list (list-ref ones (- n 81)))))
       ((< 90 (abs n) 100)
        (cons 'ninety (list (list-ref ones (- n 91)))))
       ((>= n 100) '(error)) ;;move to top?
       ((integer? (quotient n 10))
        (list (list-ref tens (- (quotient n 10) 2))))
       (else '(ERROR: CANT RETURN RESULT)) ;; if no case found for n. UNLIKELY
       ;; END POSITIVE READOUT
       ) ;; END SECOND COND
     ) ;;END TOP ELSE
  );; END TOP COND
);;END to-words       

;;TEST BENCH (to-words)
;; * means working
;; $ means not working.
;;(to-words -13) *
;;(to-words 13) *
;;(to-words 56) *
;;(to-words -24) *
;;(to-words 10) *
;;(to-words 40) *

;;------------------------------------------------------------------------