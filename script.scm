(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))

(define (get key-1 key-2)
  ((operation-table 'lookup-proc) key-1 key-2))

(define (put key-1 key-2 value)
  ((operation-table 'insert-proc!) key-1 key-2 value))


;; functions above this comment are assumed and not a part of current exercise


(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))

(define (make-exponentiation base exponent)
  (cond
    ((and (number? exponent) (= exponent 0)) 1)
    ((and (number? exponent)(= exponent 1)) base)
    ((and (number? base ) (= base 0)) 0);unless exponent = 0
    (else (list '** base exponent))))

(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2)) (else (list '* m1 m2))))

(define (=number? exp num) (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (car s));assume s is operands pair
(define (augend s) (cadr s));assume s is operands pair

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (car p)) ;assume p is operands pair
(define (multiplicand p) (cadr p)) ;assume p is operands pair

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (car e));assume e is pair of operands
(define (exponent e) (cadr e));assume e is pair of operands

(define (install-deriv-package)

  (define (sum-deriv operands var) 
    (make-sum (deriv (augend operands) var) (deriv (addend operands) var)))

  (define (product-deriv operands var) 
    (make-sum
      (make-product (multiplier operands) (deriv (multiplicand operands) var))
      (make-product (multiplicand operands) (deriv (multiplier operands) var))))

  (define (exponentiation-deriv operands var)
    (make-product
      (exponent operands)
      (make-product
          (make-exponentiation (base operands) (- (exponent operands) 1))
          (deriv (base operands) var))))

  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-deriv)
  (put 'deriv '** exponentiation-deriv))

(install-deriv-package)

(define (deriv exp var)
  ;(display "\nderiv ") (display exp)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else ((get 'deriv (operator exp))
      (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(deriv '(* (+ x y) (+ x 3)) 'x)

(define expr (make-exponentiation 'x 13))
(exponentiation? expr)
(sum? expr)

(deriv expr 'x)