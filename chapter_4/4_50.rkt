#lang racket

;; add to analyze
((ramb? exp) (analyze-ramb exp))

;; analyze-ramb
(define (analyze-ramb exp)
  (analyze-amb (cons 'amb (ramb-choices exp))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (shuffle (cdr exp)))

;;shuffle

(define (shuffle lst)
  (define (grab-n source n)
    (if (= n 0)
        (car source)
        (grab-n (cdr source) (- n 1))))
  (define (clean-n source n)
    (if (= n 0)
        (cdr source)
        (cons (car source) (clean-n (cdr source) (- n 1)))))
  (define (internal source result)
    (if (pair? source)
        (let ((pos (random (length source))))
          (cons (grab-n source pos) (internal (clean-n source pos) result)))
        '()))
    
  (internal lst '()))
  
