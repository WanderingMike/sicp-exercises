#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (sum-deriv exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (product-deriv exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))

(put 'sum 'deriv sum-deriv)

(put 'product 'deriv product-deriv)

;; solution
(define (install-sum-package)
  (define (addend s)
    (cadr s))
  (define (augend s)
    (caddr s))
  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'deriv '(+) sum-deriv)
  (put 'make-sum '+ make-sum)
  'done)

(define (make-sum add aug)
  ((get 'make-sum '+) add aug))


    
