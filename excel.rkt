#lang racket
(require racket/trace)

(define english '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define (cardinal-num sym lst)
  (cond
    [(empty? lst) #f]
    [(equal? sym (car lst)) 1]
    [else (+ 1 (cardinal-num sym (cdr lst)))]))

(define (is-in? obj lst)
  (cond
    [(empty? lst) #f]
    [(equal? obj (car lst)) #t]
    [else (is-in? obj (cdr lst))]))

(define (permissible? str alph)
  (cond
    [(empty? str) #t]
    [(is-in? (car str) alph) (permissible? (cdr str) alph)]
    [else #f]))

(define (trans-to-nums str alph)
  (cond
    [(empty? str) str]
    [(cons (cardinal-num (car str) alph) (trans-to-nums (cdr str) alph))]))

(define (trans-aux str alph iters)
  (let [(nums (reverse (trans-to-nums str alph)))]
    (cond
      [(not (permissible? str alph)) #f]
      [(and (= (length nums) 1) (= iters 0)) (car nums)]
      [(and (= (length nums) 1) (> iters 0)) (* (expt (length alph) iters) (car nums))]
      [else (+ (* (expt (length alph) iters) (car nums)) (trans-aux (reverse (cdr (reverse str))) alph (+ 1 iters)))])))
      
(define (trans str)
  (trans-aux str english 0))