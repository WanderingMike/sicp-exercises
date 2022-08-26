#lang racket

(define (same-parity . z)
  
  (define (test func input)
    (if (null? input)
        (list)
        (if (func (car input))
            (cons (car input)(test func (cdr input)))
            (test func (cdr input)))))
    
  (if (even? (car z))
      (test even? z)
      (test odd? z)))


(same-parity 2 4 4 5 6 4 2 3 4 6)