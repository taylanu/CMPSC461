;; CMPSC 461 Project 2 Taylan Unal (tuu2)
;;------------------------------------------------------------------------

;; #1  higher order function dncall
;; returns the result of calling f on x, (2 * n) times.
;; for example, if n=2, function is addOne, and x=2. dncall will perform addOne(x=2) twice.

(define (addOne x) (+ x 1))
         
(define (dncall n f x) ;; 3 parameters, one n value, one 
  (if (= n 0)
      x
      (dncall (- n 1) f (f (f x)))
   )
)
;; TESTBENCH (dncall)
;; (dncall 2 addOne 2) [Should return 6]
;; (dncall 2 addOne 3) [Should return 7]
;; (dncall 3 addOne 2) [Should return 8]

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
         
;; #5 Develop scheme program that calculates relevant word counts
;; Given two lists. One with words to count, other with irrelevant words
;; Essentially have to diff the two lists, if I have a word in list1 that is irrelevant, then subtract that from the word count

;;------------------------------------------------------------------------

;; #5a write a function filterWords that takes a list of words and irrelevant words, returns an output list with irrelevant words removed
(define

;;------------------------------------------------------------------------

;; #5b write a function mergeWordCounts, takes in two inputs
;; First input is word-count pair and second is word-count list
;; function will generate a new word-count list. If input

;;------------------------------------------------------------------------

;; #5c 