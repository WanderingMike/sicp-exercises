#lang racket

(define (smooth f)
  (define dx 0.01)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smoothed f n)
  (cond ((= n 0) f)
        (else (n-fold-smoothed (smooth f) (- n 1)))))

(define (cube x) (* x x x))

