#lang racket

(define (invert-unit-series S)
  (cons-stream 1
               (stream-scale -1 (mul-series (stream-cdr S) (invert-unit-series S)))))