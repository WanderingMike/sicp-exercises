#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'C 1)
                                   (make-leaf 'D 1)))))

(define (inside-set symbol list-symbols)
  (cond ((null? list-symbols) #f)
        ((eq? (car list-symbols) symbol) #t)
        (else (inside-set symbol (cdr list-symbols)))))

(define (encode-symbol2 symbol tree)
  (cond ((leaf? tree) nil)
        ((inside-set symbol (symbols (car tree)))
         (append (list 0) (encode-symbol2 symbol (car tree))))
        ((inside-set symbol (symbols (cadr tree)))
         (append (list 1) (encode-symbol2 symbol (cadr tree))))))

;; solution
(define (encode-symbol sym tree)
  (if (leaf? tree)
      (if (eq? sym (symbol-leaf tree))
          '()
          (error "missing symbol: ENCODE-SYMBOL" sym))
      (let ((left (left-branch tree)))
        (if (memq sym (symbols left))
            (cons 0 (encode-symbol sym left))
            (cons 1 (encode-symbol sym (right-branch tree)))))))


(define (encode message tree)
  (if (null? message)
      nil
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(encode (list 'D 'C 'A 'D) sample-tree)

