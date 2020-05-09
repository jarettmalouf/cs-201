#lang racket

(provide hours
	 my-last
	 my-reverse
	 my-remove
	 sorted?
 	 inflate
	 iterate
	 add5
	 collatz
	 compound
	 power-set
	 primes
	 prime-factors
 	 all-factors
	 )

(require racket/trace)

; Name: Jarett Malouf
; Email address: jarett.malouf@yale.edu

; ** problem 0 ** (1 easy point) 

(define hours 7)

; ********************************************************
; ** problem 1 ** (9 points)

(define (my-last lst)
  (cond
    [(empty? lst) empty]
    [(= (length lst) 1) (car lst)]
    [else
      (my-last (cdr lst))]))

; ********************************************************
; ** problem 2 ** (10 points)

(define (my-reverse lst)
  (define (myr-aux lst result)
    (if (empty? lst) result
    (myr-aux (cdr lst) (cons (car lst) result))))
  (myr-aux lst '()))

; ********************************************************
; ** problem 3 ** (10 points)

(define (my-remove value lst [proc equal?])
  (if (empty? lst) empty
  (if (proc value (car lst))
      ;remove it
      (cdr lst)
      ;keep it
      (cons (car lst) (my-remove value (cdr lst) proc)))))

; ********************************************************
; ** problem 4 ** (10 points)

(define (sorted? lst . compare?)
  (let ([op
         (if (null? compare?) <=
         (car compare?))])
  (cond
    [(empty? lst) #t]
    [(= (length lst) 1) #t]
    [else
     (if (op (car lst) (car (cdr lst)))
         (sorted? (cdr lst) op)
         #f)])))

; ********************************************************
; ** problem 5 ** (10 points)

(define (inflate lst)
  (if (empty? lst) empty
  (if (number? (car lst))
      (cons (+ (car lst) 1) (inflate (cdr lst)))
      (cons (car lst) (inflate (cdr lst))))))

; ********************************************************
; ** problem 6 ** (10 points)

(define (add5 x) (+ x 5))

(define (collatz n)
  (if (= (modulo n 2) 0) (/ n 2)
      (+ 1 (* n 3))))

(define (iterate start proc n)
  (if (= n 0) empty
      (cons (proc start) (iterate (proc start) proc (- n 1)))))

; ********************************************************
; ** problem 7 ** (15 points)

(define (compound start proc test)
  (if (test start) empty
      (cons (proc start) (compound (proc start) proc test))))

; ********************************************************
; ** problem 8 (15 points)

(define (power-set lst)
  (let [(pwsetmini (if (empty? lst) empty (power-set (cdr lst))))]
  (cond
    [(empty? lst) (list lst)]
    [else
     (append pwsetmini (map (lambda (x) (cons (car lst) x)) pwsetmini))])))
      
; ********************************************************
; ** problem 9 (10 points)

(define (primes n)
  (define (sift list p)
    (filter (lambda (n)
              (not (zero? (modulo n p))))
            list))
  (define (iter nums primes)
    (let ((p (car nums)))
      (if (> (* p p) n)
          (append (reverse primes) nums)
          (iter (sift (cdr nums) p) (cons p primes)))))
  (iter (cdr (build-list n add1)) '()))

(define (divides? p q)
  (zero? (modulo q p)))

(define (prime-factors n)
  (let loop ((primes (primes n)))
    (cond ((memq n primes) (list n))
          ((divides? (car primes) n)
           (cons (car primes) (prime-factors (/ n (car primes)))))
          (else (loop (cdr primes))))))

(define (all-factors n)
  (define (prod lst)
    (if (= (length lst) 1) (car lst)
        (* (car lst) (prod (cdr lst)))))
  (if (= n 1) '(1)
    (sort (append '(1) (map prod (remove-duplicates (my-remove '() (power-set (prime-factors n)))))) < )))

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))
	
(test 'my-last (my-last '(1 2 3 4 5 6 7)) 7)
(test 'my-last (my-last '(1)) 1)
(test 'my-last (my-last '()) '())
(test 'my-last (my-last '((1) (2) (3))) '(3))

(test 'my-reverse (my-reverse '(1 2 3 4)) '(4 3 2 1))
(test 'my-reverse (my-reverse '(1)) '(1))
(test 'my-reverse (my-reverse '()) '())
(test 'my-reverse (my-reverse '((1) (2) (3))) '((3) (2) (1)))


(test 'my-remove (my-remove 2 '(1 2 3 2 4)) '(1 3 2 4))
(test 'my-remove (my-remove 2 '(1 2 3 2 4) =) '(1 3 2 4))
(test 'my-remove (my-remove '(2) '((1) (2) (3))) '((1) (3)))
(test 'my-remove (my-remove "2" '("1" "2" "3")) '("1" "3"))
(test 'my-remove (my-remove #\c '(#\a #\b #\c)) '(#\a #\b))
(test 'my-remove (my-remove 2 '(1 2 3 4) <) '(1 2 4))
(test 'my-remove (my-remove 2 '(1 2 3 4) >) '(2 3 4))


(test 'sorted? (sorted? '(1 2 3 4) <) #t)
(test 'sorted? (sorted? '(1 2 3 4) >) #f)
(test 'sorted? (sorted? '(1 2 3 4 4) <) #f)
(test 'sorted? (sorted? '(1 1 1 1) =) #t)
(test 'sorted? (sorted? '(1 1 1 1) <) #f)
(test 'sorted? (sorted? '(1 1 1 1) <=) #t)
(test 'sorted? (sorted? '("a" "b" "c") string<=?) #t)
(test 'sorted? (sorted? '((1) (1 2) (1 2 3) (1 2 3 4)) (lambda (x y) (<= (length x) (length y)))) #t)
(test 'sorted? (sorted? '((1) (1 2) (1 2 3) (1 2 3 4) (1)) (lambda (x y) (<= (length x) (length y)))) #f)

(test 'sorted? (sorted? '(1 2 3 4)) #t)
(test 'sorted? (sorted? '(1 2 3 4 4 4)) #t)
(test 'sorted? (sorted? '(1 2 3 4 3 2 1)) #f)

(test 'inflate (inflate '(1 2 3)) '(2 3 4))
(test 'inflate (inflate '(1)) '(2))
(test 'inflate (inflate '()) '())
(test 'inflate (inflate '(a b c 2 3 4)) '(a b c 3 4 5))
(test 'inflate (inflate '((1) (2) (3))) '((1) (2) (3)))


(test 'iterate (iterate 2 add5 10) '(7 12 17 22 27 32 37 42 47 52))
(test 'iterate (iterate 0 (lambda (x) (+ x 1)) 3) '(1 2 3))
(test 'iterate (iterate 1 (lambda (n) (* n 2)) 10) '(2 4 8 16 32 64 128 256 512 1024))
(test 'iterate (iterate 1 (lambda (x) (* x -2)) 10) '(-2 4 -8 16 -32 64 -128 256 -512 1024))
(test 'iterate (iterate 10 (lambda (n) (- n 1)) 10) '(9 8 7 6 5 4 3 2 1 0))
(test 'iterate (iterate 3 (lambda (n) (+ n 2)) 10) '(5 7 9 11 13 15 17 19 21 23))
(test 'iterate (iterate 100 collatz 25) '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))


(test 'compound (compound 100 collatz (lambda (x) (= x 1))) '(50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
(test 'compound (compound 200 collatz (lambda (x) (= x 1)))  '(100 50 25 76 38 19 58 29 88 44 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
(test 'compound (compound 256 collatz (lambda (x) (= x 1)))  '(128 64 32 16 8 4 2 1))

(test 'compound (compound 10 (lambda (n) (- n 1)) (lambda (n) (<= n 0))) '(9 8 7 6 5 4 3 2 1 0))
(test 'compound (compound 0 add5 (lambda (x) (> x 50))) '(5 10 15 20 25 30 35 40 45 50 55))
(test 'compound (compound 0 add5 (lambda (x) (>= x 50))) '(5 10 15 20 25 30 35 40 45 50))
(test 'compound (compound 2 (lambda (n) (* n 2)) (lambda (x) (>= x 50))) '(4 8 16 32 64))

(test 'power-set (power-set '()) '(()))
(test 'power-set (power-set '(1)) '(() (1)))
(test 'power-set (power-set '(1 2)) '(() (2) (1) (1 2)))
(test 'power-set (power-set '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))

(test 'all-factors (all-factors 20) '(1 2 4 5 10 20))
(test 'all-factors (all-factors 32) '(1 2 4 8 16 32))
(test 'all-factors (all-factors 97) '(1 97))
(test 'all-factors (all-factors 1000) '(1 2 4 5 8 10 20 25 40 50 100 125 200 250 500 1000))
(test 'all-factors (all-factors 30030) '(1 2 3 5 6 7 10 11 13 14 15 21 22 26 30 33 35 39 42 55 65 66 70 77 78 91 105 110 130 143 154 165 182 195 210 231 273 286 330 385 390 429 455 462 546 715 770 858 910 1001 1155 1365 1430 2002 2145 2310 2730 3003 4290 5005 6006 10010 15015 30030))


;*********************************************************
;***** end of hw #1