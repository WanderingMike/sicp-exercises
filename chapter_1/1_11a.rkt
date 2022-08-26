#lang racket

(define (weird_fib n)
  (cond ((< n 3) n)
        (else (+ (weird_fib (- n 1)) (* 2 (weird_fib (- n 2))) (* 3 (weird_fib (- n 3)))))))

(define (weird_fib_it n)
  (define (sub_func a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (else (sub_func (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (sub_func 2 1 0 (- n 2)))

(define (f n) 
    (cond ((< n 3) n) 
         (else (+ (f (- n 1)) 
                  (* 2 (f (- n 2))) 
                  (* 3 (f (- n 3))))))) 