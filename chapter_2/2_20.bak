#lang racket

(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

(define (make-point x-point y-point)
  (cons x-point y-point))

(define (average a b) (/ (+ a b) 2))

(define (midpoint-segment segment)
  (let ((x-mid (average (car(car segment)) (car(cdr segment))))
        (y-mid (average (cdr(car segment)) (cdr(cdr segment)))))
    (make-point x-mid y-mid)))


(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define seg (make-segment (make-point 2 3) (make-point 10 15)))

(print-point (midpoint-segment seg))
