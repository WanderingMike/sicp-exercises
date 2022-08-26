#lang racket
;; functional expressions
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; solution
(define (let*? expr) (tagged-list? expr 'let*))
(define (let*-bindings expr) (cadr expr)) 
(define (let*-body expr) (cddr expr))
(define (make-let args body) (cons 'let (cons args body)))

(define (let*->nested-lets expr)
  (define (unfold-let* args body)
    (if (null? args)
        (sequence->exp body)
        (make-let (list (car args))
                  (list (unfold-let* (cdr args) body)))))
  (unfold-let* (let*-bindings expr) (let*-body expr)))