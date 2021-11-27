;;------------------------------------------------------------------------

;; #3 least function
;;Define the keepif function using case analysis and recursion
;;Input: a number 'k', a list of numbers 'x'
;;Ouput: the minimum element from a list of numbers
(define (least_helper k x) ;; k is a number, x is a list of numbers
  (if (null? x) 
      k ;;is list is empty, return k, since k is least in set of k
      (if (< (car x) k)
          (least_helper (car x) (cdr x))
          (least_helper k (cdr x))
      )
   )
)

(define (least l)
  (if (null? l)
      '()
      (least_helper (car l) (cdr l)) ;;maybe try if errors (least_helper (car lst) lst))
  )
)

;; TESTBENCH (least)
;; (least '(7 3 6 2)) [should return 2]
;; (least_helper 5 '(4 5 6)) [should return 4]

;;------------------------------------------------------------------------