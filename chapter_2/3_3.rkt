#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Wrong password, fuck off")))
  dispatch)

  
 (define (make-account2 balance password) 
   (define (withdraw amount) 
     (if (>= balance amount) (begin (set! balance (- balance amount)) balance) 
         "Insufficient funds")) 
   (define (deposit amount) 
     (set! balance (+ balance amount)) balance) 
   (define (dispatch p m) 
     (cond ((not (eq? p password)) (lambda (x) "Incorrect password")) 
           ((eq? m 'withdraw) withdraw) 
           ((eq? m 'deposit) deposit) 
           (else (error "Unknown request -- MAKE-ACCOUNT" m)))) 
   dispatch) 

(define acc (make-account2 100 'helloworld))

((acc 'helloorld 'withdraw) 'x)