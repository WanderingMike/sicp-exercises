#lang racket

(define (stream-limit1 stream tolerance)
  (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
      (stream-ref stream 1)
      (stream-limit1 (stream-cdr stream) tolerance)))

;; or
 (define (sub-streams stream1 stream2) 
     (stream-map - stream1 stream2)) 
  
 (define (one-order-difference stream) 
     (sub-streams (stream-cdr stream) stream)) 
  
 (define (stream-limit stream tolerance) 
     (define (iter tolerance-stream stream) 
         (if (<= (abs (stream-car tolerance-stream)) tolerance) 
             (stream-car stream) 
             (iter (stream-cdr tolerance-stream) (stream-cdr stream)))) 
     (iter (one-order-difference stream) (stream-cdr stream))) 