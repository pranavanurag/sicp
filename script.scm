;; the main hindarance in solving 2.73 is the absence of a get and put (to table) as described in the book

(define table '())

(define already-exists?
  (lambda (key)
    (if (assoc key table) 
        #t 
        #f)))

(define update-existing
  (lambda (key value)
    (set! table (cons (cons key value) (remove-pair table key)))))

(define remove-pair
  (lambda (table key)
    (filter (lambda (item) 
              (not (eq? key (car item)))) 
            table)))

(define create-entry
  (lambda (key value)
    (set! table (cons (cons key value) table))))

(define put
  (lambda (key value)
    (if (already-exists? key)
        (update-existing key value)
        (create-entry key value))))

(define get 
  (lambda (key) 
    (cdr (assoc key table))))

; this works but the book wants
;(put ⟨op ⟩ ⟨type ⟩ ⟨item ⟩) installs the ⟨item ⟩ in the table, indexed by the ⟨op ⟩ and the ⟨type ⟩.
; (get ⟨op ⟩ ⟨type ⟩) looks up the ⟨op ⟩, ⟨type ⟩ entry in the table and returns the item found there. If no item is found, get returns false.

; my key is of a different 'type' WIP