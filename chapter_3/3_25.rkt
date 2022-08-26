#lang racket

(define (assoc key records) ;assoc returns the record that has the given key as its car. Works at the same hierarchical level
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup keys-list record)
  (let ((subtable (assoc (car keys-list) (cdr record))))
    (if subtable
        (if (null? (cdr keys-list))
            (cdr subtable)
            (lookup (cdr keys-list) (cdr subtable)))
        false)))

(define (insert! keys-list value table)
  (let ((subtable (assoc (car keys-list) (cdr table))))
    (cond ((and subtable (null? (cddr keys-list))) ;at the maximum table depth
           (let ((record (assoc (cadr keys-list) (cdr subtable))))
             (if record
                 (set-cdr! record value)
                 (set-cdr! subtable
                           (cons (cons (cadr keys-list) value)
                                 (cdr subtable)))))                                
          ((subtable) (insert! (cdr keys-list) value subtable))
          ((and (not (subtable)) (null? (cddr keys-list)))
           (set-cdr! table
                     (cons (list (car keys-list)
                                 (cons (cadr keys-list value)))
                           (cdr table))))
          ((else
            (set-cdr! table (cons (insert! (cdr keys-list) value (cons (car keys-list) '()) subtable)))))))
  'ok)
