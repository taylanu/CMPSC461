;;------------------------------------------------------------------------
      
;; #2 Scheme keep-if
;; INPUT: f is a boolean function, l is a list of ints.
;; OUTPUT: List of ints that pass condition in boolean function
(define (keep-if f l)
  (define output '())
  (cond
    ((null? l) '());; if list is null, return empty list.
    ((f (car l)) ;; apply function on first element of list.
     (cons (car l) (keep-if f (cdr l))));;if car l is true, constructs list with it and result of keep-if last element of l.
    (else (keep-if f (cdr l)))
  )
)

;; TESTBENCH (keep-if)
;; (keep-if (lambda (x) (> x 3)) '(10 1 7 2)) [should be 10 7] WORKS
;; (keep-if (lambda (x) (> x 3)) '(10 7 1 2)) [should be 10 7] WORKS
;;Define the keepif function using case analysis and recursion

;;------------------------------------------------------------------------