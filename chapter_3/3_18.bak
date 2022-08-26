#lang racket

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((area-rectangle (* (- x2 x1) (- y2 y1))))
    (* area-rectangle (monte-carlo trials (lambda () (p x1 x2 y1 y2))))))

(define (predicate x1 x2 y1 y2)
    (let ((x-point (random-in-range x1 x2))
          (y-point (random-in-range y1 y2)))
      (<= (+ (square (- x-point 0)) (square (- y-point 0))) (square 1))))

; one possible solution
(define (predicate2 x1 x2 y1 y2 radius) 
   (<= (+ (square (random-in-range x1 x2)) 
          (square (random-in-range y1 y2))) 
       radius)) 
  
(define (estimate-integral2 P trials x1 x2 y1 y2) 
   (* (monte-carlo trials (lambda () (P x1 x2 y1 y2 1))) 
      4))  
 ;; whereby 4 is the area of the rectangle containing the unit circle (@ * 2) 
  
(estimate-integral2 predicate2 100000 -1 1 -1 1)

