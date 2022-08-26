#lang sicp

(controller
 (assign x (op read)) 
 (assign guess (const 1.0)) 
 
 test-b
 (test  (op good) (reg guess) (reg guess))
 (branch (label gcd-done))
 (assign t (op improve) (reg x) (reg guess))
 (assign guess (reg t))
 (goto (label test-b))
 gcd-done

 (perform (op print) (reg guess))
 )

;; 