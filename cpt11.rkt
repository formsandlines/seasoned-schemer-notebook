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


;; my own (pragmatic) attempt
(define two-in-a-row?
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

(define two-in-a-row?
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

(define two-in-a-row?
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

(define two-in-a-row?
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

(define sum-of-prefixes
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

(define sum-of-prefixes
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
