
; 3.18: cycle detection
(define has-cycle?
  (lambda (lst)
    (letrec ((step (lambda (prev rest)
                     (cond ((null? rest) #f)
                           ((memq (car rest) prev) #t)
                           (else (step (cons (car rest) prev) (cdr rest)))))))
      (step '() lst))))

; 3.19: constant-space cycle detection
(define has-cycle-2?
  (lambda (lst)
    (letrec ((step (lambda (a b)
                     (cond ((null? b) #f)
                           ((eq? (car a) (car b)) #t)
                           (else (step (cdr a) (cdr (cdr b))))))))
      (step lst (cdr lst)))))

(define make-cycle
  (lambda ()
    (let ((a '(a b c d)))
      (set-cdr! (cdr (cdr a)) a)
      a)))