#lang racket

 ;; left to right
 (define (list-of-values1 exps env) 
   (if (no-operand? exps) 
       '() 
       (let* ((left (eval (first-operand exps) env)) 
               (rest (eval (rest-operands exps) env))) 
         (cons left rest)))) 
  
 ;; right to left 
 (define (list-of-values2 exps env) 
   (if (no-operand? exps) 
       '() 
       (let* ((right (eval (rest-operands exps) env)) 
              (left (eval (first-operand exps) env))) 
         (cons left right)))) 