#lang racket
(require rnrs/mutable-pairs-6)

(define (pop-cycle lst)
  (let ((hist '()))
    (define (run-through objects)
      (cond ((null? (cdr objects)) #f)
            ((memq (car objects) hist)#t)
            (else (set! hist (cons (car objects) hist))
                  (run-through (cdr objects)))))
    (run-through lst)))

(pop-cycle (list 'a 'b 'c))

;; if you want to go through nested lists
 (define x '(a b c)) 
 (define y '(d e f)) 
 (set-car! (cdr x) y) 
 (set-car! x (cdr x)) 
 (set-cdr! (last-pair y) (cdr y)) 
 ;list y is now part of x . Cycle is within the car, therefore your solution above doesn't work properly on nested lists.
 ;here (cycle? x) gives false, but really x is an infinite loop, because it will get stuck in (cadr x) pointer which contains the implicit loop in y, thus (cddr x) will never be evaluated. 


(define (inf_loop? L) 
   (define (iter items trav) 
     (cond ((not (pair? items)) #f) 
           ((eq? (cdr items) items) #t) 
           ((eq? (car items) (cdr items)) 
            (iter (cdr items) trav)) 
           ((element-of-set? (car items) trav) #t) 
           ((element-of-set? (cdr items) trav) #t) 
           (else  
            (if (not (pair? (car items))) 
                (iter (cdr items) (cons items trav)) 
                (or (iter (car items) (cons items trav)) 
                    (iter (cdr items) (cons items trav))))))) 
   (iter L '()))

;; For constant space, think of two pointers, one which moves faster than the other. If there is a cycle, the faster one will "catch up" to the slower one. Problem was studied by Robert Floyd of the Floyd-Warschall algorithm.
(define (contains-cycle? lst) 
   (define (safe-cdr l) 
     (if (pair? l) 
         (cdr l) 
         '())) 
   (define (iter a b) 
     (cond ((not (pair? a)) #f) 
           ((not (pair? b)) #f) 
           ((eq? a b) #t) 
           ((eq? a (safe-cdr b)) #t) 
           (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
   (iter (safe-cdr lst) (safe-cdr (safe-cdr lst)))) 