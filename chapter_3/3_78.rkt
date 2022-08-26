#lang racket


;; my attempt
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-add (scale-stream dy a) (scale-stream y b)))
  y)

;; general solution
(define(general-solve-2nd f y0 dy0 dt) 
  (define y (integral (delay dy) y0 dt)) 
  (define dy (integral (delay ddy) dy0 dt)) 
  (define ddy (stream-map f dy y)) 
  y) 