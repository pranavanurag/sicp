(define (make-account balance password)
  (define failed-attempts 0)

  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define call-the-cops
    "Any gangster will tell you that the strain and anxiety of busting a safe are greatly diminished if you've a look-out man ready at any moment to say 'Cheese it, the cops!'")

  (define (deny amount)
      (if (>= failed-attempts 7)
        call-the-cops
        (begin
          (newline) (display "amount invoked, failed attempts = ") (display failed-attempts)
          (set! failed-attempts (+ 1 failed-attempts))
          "Incorrect password")))
    
  (define (dispatch p m)
    (if (eq? password p)
      (begin
        (set! failed-attempts 0)
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
      deny))

  dispatch)

(define myaccount (make-account 100 'king-crimson))
((myaccount 'king-crimson 'withdraw) 70)
((myaccount 'aphex-crimson 'withdraw) 100)
((myaccount 'aphex-crimson 'withdraw) 100)
((myaccount 'aphex-crimson 'withdraw) 100)
((myaccount 'aphex-crimson 'withdraw) 100)
((myaccount 'aphex-crimson 'withdraw) 100)
((myaccount 'aphex-crimson 'withdraw) 100)
((myaccount 'aphex-crimson 'withdraw) 100)
((myaccount 'aphex-crimson 'withdraw) 100)
((myaccount 'aphex-crimson 'withdraw) 100)