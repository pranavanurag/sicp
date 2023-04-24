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

(define myaccount (make-account 100 'king-crimson))
((myaccount 'king-crimson 'withdraw) 70)
((myaccount 'aphex-crimson 'withdraw) 100)