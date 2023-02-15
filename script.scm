(define x (list (list 1 2) (list 3 4))) x

(define (reverse some-list)
  ;(newline) (display "reverse invoked with ") (display some-list)
  (define (reverse-iter remaining ans)
    (cond
      ((null? remaining) ans)
      ((not (pair? remaining)) remaining)
      (else (reverse-iter
        (cdr remaining)
        (cons (reverse (car remaining)) ans)))))
  (reverse-iter some-list '()))

(reverse x)

(reverse (list (list 3 4) (list 3 4 5 6 1 3)))
