(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons
      (accumulate op init (map (lambda (x) (car x)) seqs))
      (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12) (list 13 14 15 16)))