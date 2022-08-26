#lang racket

(define (no-conflict col board)
  (define (iter next k)
    (or (null? next)
        (and (not (= (car next) col))
             (not (= (car next) (- col k)))
             (not (= (car next) (+ col k)))
             (iter (cdr next) (+ k 1)))))
  (iter board 1))
  
(define (queens n)
  (define (iter row results)
    (if (= row n)
        (display results)
        (let ((col (an-integer-between 0 (- n 1))))
          (require (no-conflict col results))
          (iter (+ row 1) (cons col results)))))

   (iter 0 '()))