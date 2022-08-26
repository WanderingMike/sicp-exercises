#lang racket

;;a
((lambda (n)
  ((lambda (fact)
  (fact fact n))
 (lambda (ft k)
   (if (= k 1)
       1
       (* k (ft ft (- k 1)))))))
  10)

 ((lambda (n) 
    ((lambda (fib) 
       (fib fib 1 0 n)) 
     (lambda (fib a b count) 
       (if (= count 0) 
           b 
           (fib fib (+ a b) a (- count 1)))))) 
  10) 


;;b

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 5)
(f 6)


