#lang racket

(define (make-accumulator base)
  (define (dispatch m)
    (begin (set! base (+ base m))
           base))
  
 dispatch)

  

(define A (make-accumulator 5))

(A 10)
(A 100)