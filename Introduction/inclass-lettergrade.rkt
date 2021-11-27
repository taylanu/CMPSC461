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