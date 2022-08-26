#lang racket

(define (yachts)
  (define gab 'gabrielle) 
  (define lor 'lorna) 
  (define ros 'rosalind) 
  (define mel 'melissa) 
  (define mar 'mary-ann)
  (let ((barnacle (amb gab lor ros mel mar)))
    (require (eq? barnacle mel))
    (let ((moore (amb gab lor ros mel mar)))
      (require (eq? moore mar))
      (let ((hall (amb gab lor ros mel mar)))
        (require (not (memq hall (list barnacle moore ros))))
        (let ((downing (amb gab lor ros mel mar)))
          (require (not (memq downing (list barnacle hall moore mel))))
          (let ((parker (amb gab lor ros mel mar)))
            (require (not memq parker (list barnacle moore hall downing mar)))

          (let ((yacht-names
                 (list (list gab barnacle)
                       (list lor moore)
                       (list ros hall)
                       (list mel downing)
                       (list mar parker))))
            (require (eq? parker (cadr (assq gab yacht-names)))) ;;assq peruses name-objects listings and retrieves the correct pair.
            (list (list 'barnacle barnacle) 
                     (list 'moore moore) 
                     (list 'hall hall) 
                     (list 'downing downing) 
                     (list 'parker parker))))))))) 
    
    