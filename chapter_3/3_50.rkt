#lang racket

(define test1 (lambda() (display "hello"))) ;this just assigns the procedure to an object, it doesn't run it
test1

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda (x) ;; we have a second lambda, cause we want to execute the first one only once!
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define test (memo-proc (lambda () (display "hello"))))
test

(force test)
