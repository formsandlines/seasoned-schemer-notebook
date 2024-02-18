#lang racket

(require "lib.rkt")


;; Chapter 11

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))


(define two-in-a-row? null)

;; my own (pragmatic) attempt
(set! two-in-a-row?
  (lambda (lat)
    (cond
      ((or (null? (cdr lat))
           (null? lat)) #f)
      (else (or (eq? (car lat) (cadr lat))
                (two-in-a-row? (cdr lat)))))))

(eq? #t (two-in-a-row? '(Italian sardines sardines spaghetti parsley)))
(eq? #f (two-in-a-row? '(Italian sardines more sardines spaghetti)))


;; the extra helper function seems unnecessary to me
;; and a weird choice conceptually
(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? (car lat) a)))))

(set! two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (or (is-first? (car lat) (cdr lat))
                (two-in-a-row? (cdr lat)))))))


;; this mutual recursion makes things even more confusing
(define is-first-b?
  (lambda (a lat f)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (two-in-a-row? lat))))))

(set! two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (is-first-b? (car lat) (cdr lat))))))


;; the next step is conceptually much clearer and makes more sense,
;; but I find the derivation quite confusing and artificial
;; also, the naming of the auxilliary function is bad
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) preceding)
                (two-in-a-row-b? (car lat) (cdr lat)))))))

(set! two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))


;; my own attempt, trying to avoid duplication
(define sum-of-prefixes--aux
  (lambda (preceding tup)
    (cond
      ((null? tup) (list preceding))
      (else (cons preceding
                  (sum-of-prefixes--aux (+ preceding (car tup))
                                        (cdr tup)))))))

(define sum-of-prefixes null)

(set! sum-of-prefixes
  (lambda (tup)
    (cond
      ((null? tup) '())
      (else (sum-of-prefixes--aux (car tup) (cdr tup))))))


;; this has a similar structure to two-in-a-row
(define sum-of-prefixes-b
  (lambda (sonssf tup) ;; sonssf → sum of numbers seen so far
    (cond
      ((null? tup) '())
      (else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b (+ sonssf (car tup))
                                     (cdr tup)))))))

(set! sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup))) ;; ← starting with 0 is smart


(equal? (sum-of-prefixes '())
        '())

(equal? (sum-of-prefixes '(1))
        '(1))

(equal? (sum-of-prefixes '(1 2))
        '(1 3))

(equal? (sum-of-prefixes '(1 2 3))
        '(1 3 6))

(equal? (sum-of-prefixes '(2 1 9 17 0))
        '(2 3 12 29 29))

(equal? (sum-of-prefixes '(1 1 1 1 1))
        '(1 2 3 4 5))


;; my own attempt, although in the book this function is not defined
(define prefix
  (lambda (tup l)
    (cond
      ((null? l) '())
      ((equal? tup l) (cons (car l) '())) ;; not strictly following the
                                          ;; commandments there
      (else (cons (car l)
                  (prefix tup (cdr l)))))))

(equal? (prefix '() '())
        '())

(equal? (prefix '() '(1))
        '(1))

(equal? (prefix '(1) '(1))
        '(1))

(equal? (prefix '(1 2 3) '(1 2 3))
        '(1))

(equal? (prefix '(4 2 1 1 9 2)
                '(1 1 1 3 4 2 1 1 9 2))
        '(1 1 1 3 4))

(equal? (prefix '(2 1 1 9 2)
                '(1 1 1 3 4 2 1 1 9 2))
        '(1 1 1 3 4 2))


#|
The function scramble takes a non-empty tup in which no number is greater than
its own index, and returns a tup of the same length.

Each number in the argument is treated as a backward index from its own
position to a point earlier in the tup.

The result at each position is found by counting backward from the current
position according to this index up until it reaches 1 (index start). Then the
number at that position is its value in the new list.
|#
(define scramble null)
(define scramble-b null)


;; my own attempt; forgot that `pick` was defined in chapter 4, which
;; makes this considerably easier

(define get-atom-from-backwards-index
  (lambda (i lat)
    (cond
      ((zero? (o- (length lat) i)) (car lat))
      (else (get-atom-from-backwards-index i (cdr lat))))))

(eq? (get-atom-from-backwards-index 3 '(a b c d e))
     'c)

(eq? (get-atom-from-backwards-index 1 '(a))
     'a)

(set! scramble-b
  (lambda (tup rst)
    (cond
      ((null? rst) '())
      (else (cons (get-atom-from-backwards-index (car rst) (prefix rst tup))
                  (scramble-b tup (cdr rst)))))))

(set! scramble
  (lambda (tup)
    (scramble-b tup tup)))


;; book solution: makes use of the fact that `cons` builds the list that
;; is being passed as an argument in reverse
(set! scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else (cons (pick (car tup) (cons (car tup) rev-pre))
                  (scramble-b (cdr tup)
                              (cons (car tup) rev-pre)))))))

(set! scramble
  (lambda (tup)
    (scramble-b tup '())))

(equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
        '(1 1 1 1 1 4 1 1 1 9))

(equal? (scramble '(1 2 3 4 5 6 7 8 9))
        '(1 1 1 1 1 1 1 1 1))

(equal? (scramble '(1 2 3 1 2 3 4 1 8 2 10))
        '(1 1 1 1 1 1 1 1 2 8 2))

