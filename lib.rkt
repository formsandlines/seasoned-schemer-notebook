#lang racket


;; -----------------------------------------------------
;; from chapter 4 (The Little Schemer)

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (o- n (sub1 m)))))))

(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (o+ n (sub1 m)))))))

(define ×
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (o+ n (× n (sub1 m)))))))

(= 2 (o- 5 3))
(= 8 (o+ 5 3))
(= 15 (× 5 3))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(= 1 (pick 4 '(4 3 1 1 1)))
(= 4 (pick 2 '(2 4 3 1 1 1)))
;; -----------------------------------------------------


(provide o- o+ × pick)
