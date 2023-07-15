(define (deriv exp var)
  (cond
    ((number? exp ) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else ((get 'deriv (operator exp)) (operands exp) var))))


;a. conditionals on the kinds of operators that the deriv procedure may handle
;   are now kept separate from this procedures body
;b.
(define (install-deriv-procedures)
  ((put 'deriv '+ deriv-sum)
   (put 'deriv '* deriv-product)))

(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-sum (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))
            (make-product
             (deriv (multiplier exp) var)
             (multiplicand exp))))

(install-deriv-procedures)