
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

;; 3.3.2: Representing queues

(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (make-queue) (cons '() '()))

(define (front-queue q)
  (if (empty-queue? q)
      (error "front-queue called on empty queue" q)
      (car (front-ptr q))))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
	   (set-front-ptr! q new-pair)
	   (set-rear-ptr! q new-pair))
	  (else
	   (set-cdr! (rear-ptr q) new-pair)
	   (set-rear-ptr! q new-pair))))
  q)

(define (delete-queue! q)
  (let ((first (front-queue q)))
    (set-front-ptr! q (cdr (front-ptr q)))
    q))

;; 3.23: deque
(define (make-deque)
  (cons '() '()))

(define (empty-deque? d)
  (null? (car d)))

(define (make-deque-node item prev next)
  (cons item (cons prev next)))

(define deque-node-item car)
(define deque-node-prev cadr)
(define deque-node-next cddr)

(define (set-deque-node-prev! node prev)
  (set-car! (cdr node) prev)
  node)

(define (set-deque-node-next! node next)
  (set-cdr! (cdr node) next)
  node)

(define (front-deque d)
  (deque-node-item (car d)))

(define (rear-deque d)
  (deque-node-item (cdr d)))

(define (front-insert-deque! d v)
  (let* ((old-front (car d))
         (node (make-deque-node v '() old-front)))
    (when old-front
      (set-deque-node-prev! old-front node))
    (set-car! d node)
    d))
    
