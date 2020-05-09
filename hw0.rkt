#lang racket

(provide hours
	 bmi
	 zodiac)

; problem 0
(define hours 1)

; problem 1
(define (bmi mass height)
  (floor (/ (* 703 mass) (* height height))))

; problem 2
(define (zodiac) "Virgo")

; tester
(define *testing-flag* #t)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (if (expected got) 'OK-TEST
				  'FAILED-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'hours hours (lambda (x) (> x 0)))
(test 'bmi (bmi 150 70) 21)
(test 'bmi (bmi 200 70) 28)
(test 'bmi (bmi 250 60) 48)
(test 'bmi (bmi 150 80) 16)
(test 'zodiac (zodiac) (lambda (x) (string? x)))