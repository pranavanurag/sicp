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

(fold-left + 0 (list 1 2 3 4))
(fold-right + 0 (list 1 2 3 4))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

(fold-right cons '() (list 1 2 3))
(fold-left cons '() (list 1 2 3))

(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))



(fold-right + 1 (list 1 2 3))
(fold-left + 1 (list 1 2 3))


(fold-right - 1 (list 1 2 3))
(fold-left - 1 (list 1 2 3))


(fold-right append '() (list (list 1 2 3) (list 4 5 6)))
(fold-left append '() (list (list 1 2 3) (list 4 5 6)))


(fold-right (lambda (x y) (+ 1 (* x y))) 1 (list 1 2 3))
(fold-left (lambda (x y) (+ 1 (* x y))) 1 (list 1 2 3))