#lang racket

(define nil '())

 (define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence))))) 

(define (accumulate-n op init seqs)
  (display seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

 (define t (list (list 1 2 3) (list 40 50 60) (list 700 800 900)))

(accumulate-n + 0 t)