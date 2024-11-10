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
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag tagged-data) (car tagged-data))
(define (contents tagged-data) (cdr tagged-data))

;;;;;;;;;;;
(define set-of-records-div-1
  (list
    '("Pranav Anurag" . ((sex . "M") (salary . 120) (address . "address1")))
    '("Anurag Subhash" . ((sex . "M") (salary . 420) (address . "address1_0")))
    '("Anvita Anurag" . ((sex . "F") (salary . 220) (address . "address1_1")))))

(define (key-div-1 kv-record) (car kv-record))
(define (record-div-1 kv-record) (cdr kv-record))
(define (get-record-div-1 given-key set-of-records)
  (cond
    ((null? set-of-records) #f)
    ((equal? (key-div-1 (car set-of-records)) given-key) (record-div-1 (car set-of-records)))
    (else (get-record-div-1 given-key (cdr set-of-records)))))

(define (field-getter-div-1 field-name)
  (lambda (record)
    (define (field-finder field-name record-iterator)
      (cond
        ((null? record-iterator) #f)
        ((eq? field-name (caar record-iterator)) (cdar record-iterator))
        (else (field-finder field-name (cdr record-iterator)))))
    (field-finder field-name record)))

(define tagged-records-div-1 (attach-tag 'div1 set-of-records-div-1))
(put 'get-record 'div1 get-record-div-1)
(put 'get-salary 'div1 (field-getter-div-1 'salary))
(put 'get-address 'div1 (field-getter-div-1 'address))
(put 'get-gender 'div1 (field-getter-div-1 'sex))

;;;;;;;;;;;
(define set-of-records-div-2
  (list
    '(("Pranav" "Anurag") . ((salary . 140) (addr . "address_2_0") (gender . "M")))
    '(("Some" "Dude") . ((salary . 130) (addr . "address_2_1") (gender . "F")))
    '(("Some" "Other" "Dude") . ((salary . 110) (addr . "address_2_2") (gender . "M")))))

(define (key-div-2 kv-record) (car kv-record))
(define (record-div-2 kv-record) (cdr kv-record))
(define (get-record-div-2 given-key set-of-records)
  (cond
    ((null? set-of-records) #f)
    ((equal? (key-div-2 (car set-of-records)) given-key) (record-div-2 (car set-of-records)))
    (else (get-record-div-2 given-key (cdr set-of-records)))))

; this happens to be the same procedure as field-getter-div-1 but I could possibly change the way I store properties in div2 and change this method only. and everything will work just fine
(define (field-getter-div-2 field-name)
  (lambda (record)
    (define (field-finder field-name record-iterator)
      (cond
        ((null? record-iterator) #f)
        ((eq? field-name (caar record-iterator)) (cdar record-iterator))
        (else (field-finder field-name (cdr record-iterator)))))
    (field-finder field-name record)))

(define tagged-records-div-2 (attach-tag 'div2 set-of-records-div-2))
(put 'get-record 'div2 get-record-div-2)
(put 'get-salary 'div2 (field-getter-div-2 'salary))
(put 'get-address 'div2 (field-getter-div-2 'addr))
(put 'get-gender 'div2 (field-getter-div-2 'gender))

;;;;;;;;;;;
(define (get-record name file)
  (let ((proc (get 'get-record (type-tag file))))
    (if proc
        (proc name (contents file))
        (error "Unknown division type"))))

(define (field-getter field-getter-id)
  (lambda (name file)
    (let ((division-type (type-tag file))
          (record (get-record name file)))
      ((get field-getter-id division-type) record))))

(define get-salary (field-getter 'get-salary))
(define get-gender (field-getter 'get-gender))
(define get-address (field-getter 'get-address))


(get-gender "Pranav Anurag" tagged-records-div-1)
