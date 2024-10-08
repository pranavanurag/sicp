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


;; suppose that each division's personnel records consist of a single file
;; which contains a set of records keyed on employees’ names
;; the structure of the set varies from division to division
;; (this means that the implementation of `set` used should be different in our example)
;; I am not going to write out two set implementations and use them to create example datasets
;; borrowing from code_report on youtube, I am going to use qouted lists to create data


;; division 1
;; a record here is a list of pairs (identifier . value) stored against a key (name of the employee)
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

(define (get-field-div-1 field-name record)
  ; (newline) (display "------") (display record) (newline)
  (cond
    ((null? record) #f)
    ((equal? field-name (caar record)) (cdar record))
    (else (get-field-div-1 field-name (cdr record)))))

(define (get-salary-div-1 record) (get-field-div-1 'salary record))
(define (get-address-div-1 record) (get-field-div-1 'address record))
(define (get-sex-div-1 record) (get-field-div-1 'sex record))

(define papa-record (get-record-div-1 "Anurag Subhash" set-of-records-div-1))
(get-salary-div-1 papa-record)
(get-sex-div-1 papa-record)
(get-address-div-1 papa-record)

; ;; division 2
; ;; a record here is a list with the id as the first element of the record list
; (define set-of-records-div-2
;   (list
;     '("id20" "addr20" "name20" 420)
;     '("id21" "addr21" "name21" 143)
;     '("id22" "addr22" "name22" 160)))

; (define (key-div-2 record) (car record))
; (define (get-record-div-2 given-key set-of-records)
;   ; (newline) (display (key-div-1 (car set-of-records)))
;   (cond
;     ((null? set-of-records) #f)
;     ((equal? (key-div-1 (car set-of-records)) given-key) (car set-of-records))
;     (else (get-record-div-1 given-key (cdr set-of-records)))))

; (put 'get-record 'div-1 get-record-div-1)
; (put 'get-record 'div-2 get-record-div-2)

; (define (get-record div-id record-id set-of-records)
;   ((get 'get-record div-id) record-id set-of-records))

; (get-record 'div-1 "Pranav Anurag" set-of-records-div-1)
; (get-record 'div-2 "id20" set-of-records-div-2)

; ;; for a) each division must supply a function to get a record

; (define (get-salary record)
; ;there should be a type-tag to figure out which division this record is from
; ;call appropriate method to find salary (division specific)
;   ())