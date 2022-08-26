#lang sicp

 (define count-leaves 
   (make-machine 
    `((car ,car) (cdr ,cdr) (pair? ,pair?) 
                 (null? ,null?) (+ ,+)) 
    '( 
      start 
        (assign n (const 0)) 
        (assign continue (label done)) 
        (save continue) 
      count-loop 
        (test (op pair?) (reg tree)) 
        (branch (label pair)) 
        (test (op null?) (reg tree)) 
        (branch (label null)) 
        (assign n (op +) (reg val) (const 1)) 
        (restore continue) 
        (goto (reg continue)) 
      cdr-loop 
        (restore tree) 
        (assign tree (op cdr) (reg tree)) 
        (goto (label count-loop)) 
      pair 
        (save tree) 
        (assign tree (op car) (reg tree))
        (assign continue (label cdr-loop))
        (save continue)
        (goto (label count-loop)) 
      null 
        (restore continue) 
        (goto (reg continue)) 
      done))) 