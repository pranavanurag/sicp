(define (reverse some-list)
  (define (reverse-iter remaining ans)
    (if (null? remaining)
      ans
      (reverse-iter (cdr remaining) (cons (car remaining) ans))))
  (reverse-iter some-list '()))

  
(define (first some-list)
  (car some-list))

(define (mod2 a) (remainder a 2))

(define (same-parity . x)
  (define (collect-parity-iter p curr ans)
    (if (null? curr)
      (reverse ans)
      (if (= p (mod2 (car curr)))
        (collect-parity-iter p (cdr curr) (cons (car curr) ans))
        (collect-parity-iter p (cdr curr) ans))))
  (let ((parity (mod2 (first x))))
    (collect-parity-iter parity x '())))

(same-parity 2 4 5 6 7 8 9 10 11 12 13)