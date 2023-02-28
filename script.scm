(define (accumulate op initial sequence)
  ;(newline) (display "fold-right iter invoked with: ") (display initial) (display ", ") (display sequence)
  (if (null? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (fold-right op initial sequence) (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    ;(newline) (display "fold-left iter invoked with: ") (display result) (display ", ") (display rest)
    (if (null? rest)
      result
      (iter
        (op result (car rest))
        (cdr rest))))
  (iter initial sequence))

(define (reverse-r sequence)
  (fold-right (lambda (current-element rest-result) (append rest-result (list current-element))) '() sequence))

(define (reverse-l sequence)
  (fold-left (lambda (current-element rest-result) (cons rest-result current-element)) '() sequence))

(reverse-r (list 1 2 3 4))
(reverse-l (list 1 2 3 4))