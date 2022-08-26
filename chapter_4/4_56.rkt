#lang racket

;; 1)
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

;; 2)
(and (salary ?person ?amount)
     (salary (Bitdiddle Ben) ?amount2)
     (lisp-value < ?amount ?amount2))

;; 3) all people who are supervised by someone who is not in the computer division, together with the supervisor's name and job.
(and (not (job ?boss (computer . ?type)))
     (supervisor ?person ?sup)
     (job ?sup ?job))

