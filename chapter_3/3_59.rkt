#lang racket

;; a)
(define (integrate-series S)
  (stream-map / S 1))

;; b)
(define cosine-series
  (cons-stream 1 (stream-map - (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; other way to exp
(define exp-series
  (stream-map / ones (cons-stream 1 factorials)))

