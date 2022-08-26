#lang racket

 (rule (bigshot ?person ?division) 
       (and (job ?person (?division . ?rest)) 
            (or (not (supervisor ?person ?boss)) 
                (and (supervisor ?person ?boss) 
                     (not (job ?boss (?division . ?r))) 
                     (not (bigshot ?boss ?division)))))) 