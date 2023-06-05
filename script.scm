(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (deny amount) "Incorrect password")
    
  (define (dispatch p m)
    (if (eq? password p)
      (cond
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else (error "Unknown request: MAKE-ACCOUNT" m)))
      deny))

  dispatch)

(define (make-joint master-acc master-password joint-password)
  (define (proxy p m)
    (if (eq? joint-password p)
      (master-acc master-password m)
      (error "incorrect password!")))
  proxy)

(define peter-acc (make-account 100 'kingcrimson))
((peter-acc 'kingcrimson 'withdraw) 10)
((peter-acc 'kingcrimson 'withdraw) 10)
((peter-acc 'kingcrimson 'withdraw) 10)

(define proxy-acc (make-joint peter-acc 'kingcrimson 'afx))
((proxy-acc 'afx 'withdraw) 10)