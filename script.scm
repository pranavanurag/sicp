;;;Exercise 3.8: When we defined the evaluation model in Section 1.1.3
;; we said that the first step in evaluating an expression is to evaluate its subexpressions
;; But we never specified the order in which the subexpressions should be evaluated
;; (e.g., le to right or right to le)
;; When we in- troduce assignment, the order in which the arguments to a procedure are evaluated can make a difference
;; to the result. Define a simple procedure f such that evaluating
;; (+ (f 0) (f 1))
;; will return 0 if the arguments to + are evaluated from le to right
    ;; but will return 1 if the arguments are evaluated from right to le.


;;define a scope with let inside the function
;; everytime the function is invoked this scope becomes available for the lambda that is returned by f
(define f
  (let ((seed 1))
    (lambda (x)
      (cond
        ((eq? x 0) (set! seed 0) seed)
        (else seed)))))

(f 0)
(f 1)

(define f
  (let ((seed 1))
    (lambda (x)
      (cond
        ((eq? x 0) (set! seed 0) seed)
        (else seed)))))

(f 1)
(f 0)