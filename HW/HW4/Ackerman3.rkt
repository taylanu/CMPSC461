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

#| TESTBENCH:
GOODCASES:
Input: A(3,5) Output: 253 (VERIFIED WORKING)
Input: A(0,0) Output: 1 (VERIFIED WORKING)
Input: A(2,2) Output: 7 (VERIFIED WORKING)
Input: A(2,0) Output: 3 (VERIFIED WORKING)

FAILCASES: 
Input: A(-2,-1) Output: error (VERIFIED WORKING)
Input: A(-5,-2) Output: error (VERIFIED WORKING)
|#