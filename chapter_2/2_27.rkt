#lang racket

(define x (list (list 1 2) (list 3 4 5)))

(define nil '()) 

 (define (deep-reverse y)
   (display y)
   (newline)
   (if (pair? y)
       (append (deep-reverse (cdr y)) (list (deep-reverse (car y))))
       y))

;(pair? (car (list 3 4))) will be false
;(pair? (cdr (list 3 4))) will be true because nil is at the end!
;(append (list 1) (list 2)) -> '((1) (2))
;(cons (list 1 2) (list 3 4)) -> '((1 2) 3 4)

(deep-reverse x)
(pair? (list (list 3 4) (list 2 3 5)))
(pair? (car (list 3 4)))
(pair? 1)
(append (list (list 1 3)) (list (list 2)))
