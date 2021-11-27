;; #1  higher order function dncall
;; Takes 3 paramters (n f x), returns x when n=0, when n=1 returns f(f(x)) , when n=2 returns f(f(f(f(x)))) , etc.
;; The function returns the result of calling f(x) 2n times.
;; INPUT: f is a boolean function, l is a list of ints.
;; OUTPUT: List of ints that pass condition in boolean function

(define (addOne x) (+ x 1)) ;;just a sample function, can be lambda.
         
(define (dncall n f x)
  (if (= n 0)
      x
      (dncall (- n 1) f (f (f x))) ;; else case
   )
)
;; TESTBENCH (dncall)
;; 1. (dncall 2 addOne 2) [Should return 6]
;;    If n=2, function is addOne, and x=2. dncall will perform addOne(x) (2*n = 4) times.
;;    ex. addOne(addOne(addOne(addOne(2)))) => addOne(addOne(addOne(3))) => addOne(addOne(4)) => addOne(5) => 6

;; 2. (dncall 2 addOne 3) [Should return 7]
;;    If n=2, function is addOne, and x=3. dncall will perform addOne(x) (2*n = 4) times.
;;    ex. addOne(addOne(addOne(addOne(3)))) => addOne(addOne(addOne(4))) => addOne(addOne(5)) => addOne(6) => 7

;; 3. (dncall 3 addOne 2) [Should return 8]
;;    If n=3, function is addOne, and x=2. dncall will perform addOne(x) (2*n = 6) times.
;;    ex. addOne(addOne(addOne(addOne(addOne(addOne(2))))) => addOne(addOne(addOne(addOne(addOne(3))))) => addOne(addOne(addOne(addOne(4)))) => addOne(addOne(addOne(5))) => addOne(addOne(6)) => addOne(7) => 8

;; 4. (dncall 2 (lambda (x) (* x 2)) 2) [Should return 32]
;;    If n=2, function is x*2, and x=2. dncall will perform (x*2) (2*n = 4) times.
;;    ex. times2(times2(times2(times2(2)))) => times2(times2(times2(4))) => times2(times2(8)) => times2(16) => 32
;;------------------------------------------------------------------------