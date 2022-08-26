#lang sicp

(define (attach-tag type-tag content) (cons type-tag content))

(define (get-record employee-id file)
  (attach-tag (division file)
              ((get 'get-record (division file)) employee-id file)))

;; get-salary
(define (get-salary record)
  (let ((record-type (car record))
        (record-content (cdr record)))
    ((get 'get-salary record-type) record-content)))

;; find-employee-record
(define (find-employee-record employee-id file-list)
  (if (null? file-list)
      #f
      (let ((current-file (car file-list)))
        (if (get-record employee-id current-file)
            (get-record employee-id current-file)
            (find-employee-record employee-id (cdr file-list))))))

;; When new company is taken over, it must provide an installation package for its record-file as new division.
;; This installation must include the new division get-record and get-salary implementations.