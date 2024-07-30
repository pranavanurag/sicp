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

;; as an illustration, I am going to assume that Division 1 of Insatiable Enterprises
;; stores (cons address salary)
(define (make-record-div-1 add sal) (cons add sal))
(define (address-div-1 record-div-1) (car (record-div-1)))
(define (salary-div-1 record-div-1) (cdr (record-div-1)))

;; for contrast, let's assume that Division 2 of Insatiable Enterprises
;; stores (cons salary address)
(define (make-record-div-2 sal add) (cons sal add))
(define (salary-div-2 record-div-2) (car (record-div-2)))
(define (address-div-2 record-div-2) (cdr (record-div-2)))


;; a) just give me the set bro. and just tag yo data. that's it
;; ok you will also need to give me your procedures for salary and address
(define (install-div-1)
  (define (address-div-1 record-div-1) (car (record-div-1)))
  (define (salary-div-1 record-div-1) (cdr (record-div-1)))
  (put 'div-1 'salary salary-div-1)
  (put 'div-1 'address address-div-1)
  (put 'div-1 'file ))