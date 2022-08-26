#lang racket
(require (planet soegaard/sicp:2:1/sicp))

(define nil '())

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial 
      (op (car sequence) 
          (accumulate op initial (cdr sequence))))) 

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (find-triple n s)
  (define (correct-sum? triple)
    (= (+ (car triple) (cadr triple) (caddr triple)) s))
  (define (series n)
    (flatmap
     (lambda (i)
       (flatmap (lambda (j)
              (map (lambda (k) (list i j k))
                   (enumerate-interval 1 (- j 1))))
            (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 n)))
  (define (filtering numbers)
    (filter correct-sum? numbers))
  (filtering (series n)))

(find-triple 100 100)
