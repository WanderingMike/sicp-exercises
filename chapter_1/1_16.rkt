#lang racket

(define (square x) (* x x))

(define (exp b n)
  (define (exp_iter A B N)
    (cond ((= N 0) A)
          ((even? N) (exp_iter A (square B) (/ N 2)))
          (else (exp_iter (* A B) B (- N 1)))))
  (exp_iter 1 b n))