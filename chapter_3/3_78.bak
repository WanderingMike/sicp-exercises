#lang racket


;; my attempt
(define (RC1 R C dt)
  (define (lower current voltage)
    (let ((input (scale-stream current (/ 1 C))))
       (integral input voltage dt)))
  (define (x y)
    (add-streams
     (scale-stream x R)
     (lower x y)))
  run)

;; improved

(define (RC r c dt)
  (lambda (i v0)
    (add-stream (scale-stream i r)
                (integral (scale-stream i (/ 1 C)) initial-voltage dt))))
