#lang sicp


(define (main)
  (let ((p (read)))
    (apply-arg-list EKJ p)
    (main)))

(define (apply-arg-list plane args)
  (if (null? args)
      (display "Finished processing data")
      (begin
        (apply-arg plane (car args))
        (apply-arg-list plane (cdr args)))))

(define (apply-arg plane args)
  (let ((op (car args))
        (param (cdr args)))
    ((plane op) param)))


(define (plane)
  (let ((altitude '())
        (safety '()))
    
    (define (new-altitude args)
      (let ((old-altitude altitude))
        (set! altitude (cons old-altitude args))
        (display "new altitude set")
        (display altitude)
        (newline)))
    

    (define (current-safety args)
      (let ((old-safety safety))
        (set! safety (cons old-safety args))
        (display "new safety message set")
        (newline)))

    (define (print-flight-data name)
      (cond ((eq? (car name) 'altitude) (display-list altitude))
            ((eq? (car name) 'safety) (display-list safety))
            (else (display "variable does not exist"))))

    (define (display-list lst)
      (if (not (null? lst))
          (begin
            (display (car lst))
            (display-list (cdr lst)))))
    
    (define (change-spec m)
      (cond ((eq? m 'add-altitude) new-altitude)
            ((eq? m 'safety-state) current-safety)
            ((eq? m 'print-data) print-flight-data)
            (else
             (display "Error - plane spec undefined"))))

    change-spec))

(define EKJ (plane))
(main)

	