(define (make-mobile left right) (list left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (make-branch length structure) (list length structure))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))


(define (total-weight mobile)
  (define (total-weight-iter submobile)
  ;(newline) (display "total-weight-iter called with: ") (display submobile)
    (cond
      ((null? submobile) 0)
      ((not (pair? submobile)) submobile)
      (else (+
        (total-weight-iter (branch-structure (left-branch submobile)))
        (total-weight-iter (branch-structure (right-branch submobile)))))))
  (total-weight-iter mobile))

(define x
  (make-mobile
    (make-branch 1 (make-mobile (make-branch 2 4) (make-branch 1 1)))
    (make-branch 2 3)))

(total-weight x)
