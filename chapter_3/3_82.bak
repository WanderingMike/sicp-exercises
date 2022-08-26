#lang racket


 (define (random-in-range low high) 
   (let ((range (- high low))) 
     (+ low (random range)))) 
  
 (define (rand-range-stream low high) 
   (cons-stream 
    (random-in-range low high) 
    (rand-range-stream low high))) 
  
 (define (experiment-stream x1 x2 y1 y2 radius) 
   (stream-map 
    (lambda (x) (> radius x)) 
    (add-streams 
     (stream-map square (rand-range-stream x1 x2)) 
     (stream-map square (rand-range-stream y1 y2))))) 
  
 (define pi-est-stream 
   (scale-stream (monte-carlo (experiment-stream -1.0 1.0 -1.0 1.0 1.0) 0 0) 4.0)) 
  
 (exact->inexact (stream-ref pi-est-stream 50000)) ;; ~3.1429 

