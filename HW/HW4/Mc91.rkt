;; CMPSC461 HW4 Prob3 Taylan Unal (tuu2)
;; McCarthy 91 Function
(define (Mac n)
  (cond ((> n 100) ;; CASE1 
         (- n 10))
        ((<= n 100) ;; CASE2
         (Mac (Mac (+ n 11))))
  )
)

#| TESTBENCH:
Input: 92 Output: 91 (VERIFIED)
Input: 99 Output: 91 (VERIFIED)

Input: 102 Output: 92 (VERIFIED)
Input: 104 Output: 94 (VERIFIED)
|#

#|
Cond is defined by (Case 1){Case 2) (else)
Best Explained: https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Conditionals.html

McCarthy function returns 91 up to 100, and even 101, since 101-10=91.
But after 101, each subsequent value is 91 + n. So 102 will return 92, 103 returns 93, etc.
|#
