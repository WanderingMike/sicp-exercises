#lang sicp

(define (make-table)
  (let ((table'()))

    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (put key1 key2 value)
      (let ((subtable (assoc key1 table)))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key2 value)
                                  (cdr subtable)))))
            (set! table
                  (cons (list key1
                              (cons key2 value))
                        table)))))

    (define (get key1 key2)
      (let ((subtable (assoc key1 table)))
        (if subtable
            (let ((record (assoc key2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) get)
            ((eq? m 'insert-proc!) put)
            (else (error "Operation not found -- TABLE" m))))
    
  dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


(put 'here 'hey 6)
(get 'here 'hey)

