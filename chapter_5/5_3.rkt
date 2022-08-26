#lang racket

(controller
 test-b
 (test (op <) (reg t2) (const 0.001))
 (assign t1 (op *) (reg guess) (reg guess))
 (assign t2 (op -) (reg t1) (reg x))