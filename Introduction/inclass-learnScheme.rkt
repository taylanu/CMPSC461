;;INCLASS SCHEME LESSON 3/4/20
#| Example
lettergrade(n) = {
A (if n >= 90)
B (if 80 <= n < 90)
C (if 70 <= n < 80)
D (if 60 <= n < 70)
F (if n < 60)

|#

;; INCLASS EXAMPLE OF conditional
(define (lettergrade n)
  (cond ((>= n 90) 'A) ;; 'A outputs string.
        ((>= n 80) 'B)
        ((>= n 70) 'C)
        ((>= n 60) 'D)
        (else 'F)))

;;INCLASS EXAMPLE OF case
(define (daysOfMonth m)
  (case m
    ((sep apr jun nov) 30)
    ((feb) 28)
    (else 31)))

;;INCLASS various functions
(define (twice f x) (f (f x)))
(define (plusOne x) (+ x 1))
(define (square x) (* x x))
(define (twiceV2 f) (lambda (x) (f (f x))))

#|Example usage
(twice square 3)
(twice (lambda (X) (+ x 2)) 3) == 7

(define plusFour (twiceV2 plugTwo))
(plusFour 100) == 104
(define plusEight (twiceV2 plus Four))
(plusEight 100) == 108

(twiceV2 twiceV2) ;; works, but doesnt return a value. Just tates that it is a procedure. Returns an empty lambda function
|#