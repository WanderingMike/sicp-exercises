#lang racket
(assert! (rule (replace ?person1 ?person2) 
                (and (job ?person1 ?job1) 
                     (job ?person2 ?job2) 
                     (or (same ?job1 ?job2) 
                         (can-do-job ?job1 ?job2)) 
                     (not (same ?person1 ?person2)))))

;; a)
(replace ?p (Fect Cy D))

;; b)
(and (replace ?p ?t)
     (salary ?p ?a1)
     (salary ?t ?a2)
     (lisp-value > ?a1 ?a2))
                      