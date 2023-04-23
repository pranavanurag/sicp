

(define (make-monitored f)
  (define counter 0)
  (lambda (prompt)
    (cond
      ((equal? prompt 'how-many-calls?) counter)
      ((equal? prompt 'reset-counter) (set! counter 0))
      (else
        (begin
          (set! counter (+ 1 counter))
          (f prompt))))))

(define m (make-monitored (lambda (x) (* x x))))

(m 10)
(m 10)
(m 10)
(m 'reset-counter)
(m 10)
(m 10)
(m 'how-many-calls?)