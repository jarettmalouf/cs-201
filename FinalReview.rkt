#lang racket
(require racket/trace)

(define l1 '(6 3 10 1 5))
(define l2 '())
(define l3 '(4 2 2 1 2))
(define exp1 #t)
(define exp2 'hi)
(define exp3 '(x + y))
(define exp4 '(((x + y) * (u + v)) + (a + b)))

(define (insert x lst)
  (cond
    [(null? lst) (list x)]
    [(<= x (car lst)) (cons x lst)]
    [else (cons (car lst) (insert x (cdr lst)))]))

(define (merge l1 l2)
  (cond
    [(empty? l1) l2]
    [(empty? l2) l1]
    [(<= (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2))]
    [else (cons (car l2) (merge l1 (cdr l2)))]))

(define (isort-aux lst lst-n)
  (cond
    [(empty? lst) lst-n]
    [else (isort-aux (cdr lst) (insert (car lst) lst-n))]))

(define (isort lst)
  (isort-aux lst '()))

(define (reformat exp)
  (cond
    [(or (boolean? exp) (symbol? exp)) exp]
    [else (list (cadr exp) (reformat (car exp)) (reformat (caddr exp)))]))