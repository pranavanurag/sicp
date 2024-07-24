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


(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) 1)
    (else ((get 'deriv (operator exp))
      (operands exp) var))))

(define (operator exp) (car exp))
;; I will get over this hump
;; fuck you all. you all fucking suck
(define (operands exp) (cdr exp))
;; might be helpful to copy over the boring impl for deriv from section2.3.2

(define (install-deriv-package)
  (define (internal-procedure-1 arg1 arg2) ('haha))
  
  
  (put 'deriv '(+) (crazy-function-to-handle-sums-which-takes-exp-and-var)))