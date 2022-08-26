#lang racket

(define (cont-frac n d k)
  (cond ((= 1 k) (/ (n k) (d k)))
  (else (/ (n k) (+ (d k) (cont-frac n d (- k 1)))))))

(define (find-term k)
  (cont-frac (lambda(i) 1.0) (lambda(i) 1.0) k))
