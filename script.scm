(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))



(define (make-sum . parts)
  (let
    ((num-sum (accumulate + 0 (filter number? parts)))
      (others (filter (lambda (x) (not (number? x))) parts)))
    ;(newline) (display "others = ") (display others) (display ", num-sum = ") (display num-sum)
    (cond
      ((and (null? others) (= num-sum 0)) 0)
      ((= 0 num-sum) (cons '+ others))
      ((null? others) num-sum)
      (else (newline) (append (cons '+ others) (list num-sum))))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (pair? s)
    (if (= (length s) 2)
      0
      (if (= (length s) 3)
        (caddr s)
        (cons '+ (cddr s))))
    s))
  


(define (make-product . parts)
  (let
    ((num-product (accumulate * 1 (filter number? parts)))
      (others (filter (lambda (x) (not (number? x))) parts)))
    ;(newline) (display "others = ") (display others) (display ", num-product = ") (display num-product)
    (cond
      ((and (null? others) (= num-product 0)) 0)
      ((= 0 num-product) (cons '* others))
      ((null? others) num-product)
      (else (newline) (append (cons '* others) (list num-product))))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (pair? p)
    (if (= (length p) 2)
      1
      (if (= (length p) 3)
        (caddr p)
        (cons '* (cddr p))))
    p))



(define (make-exponentiation base exponent)
  (cond
    ((and (number? exponent) (= exponent 0)) 1)
    ((and (number? exponent)(= exponent 1)) base)
    ((and (number? base )(= base 0)) 0);unless exponent = 0
    (else (list '** base exponent))))

;return true if expression = f(x)^number only
(define (exponentiation? expression var)
  (and
    (pair? expression)
    (eq? '** (car expression))
    (number? (exponent expression))))

(define (base e) (cadr e))

(define (exponent e) (caddr e))



(define (deriv exp var)
  ;(newline) (display "deriv called with: ") (display exp)
  (cond
    ((number? exp) 0)
    ((variable? exp)
      (if (same-variable? exp var) 1 0))
    ((sum? exp)
      (make-sum
        (deriv (addend exp) var)
        (deriv (augend exp) var)))
    ((product? exp)
      (make-sum
        (make-product (multiplier exp) (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var) (multiplicand exp))))
    ((exponentiation? exp var)
      (make-product
        (exponent exp)
        (make-product
          (make-exponentiation (base exp) (- (exponent exp) 1))
          (deriv (base exp) var))))
    (else (error "unknown expression type: DERIV" exp))))


(define expr '(* x x x))
(deriv expr 'x)