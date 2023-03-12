(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty-board '())

(define (adjoin-position new-queen-pos k other-queens)
  (cons new-queen-pos other-queens))


; positions are lists of size 'k', where each element lies between 0 and '(board-size - 1)'
; only the new queen at (car position) needs to be verified to be safe
(define (safe? k positions)
  (let ((new-queen (car positions)))
    (define (safe-iter col-distance remaining-queens)
      (if (null? remaining-queens)
        #t
        (cond
          ((= (car remaining-queens) new-queen) #f) ;same row
          ((= col-distance (abs (- new-queen (car remaining-queens)))) #f)  ;same diagonal
          (else (safe-iter (+ col-distance 1) (cdr remaining-queens)))))) ;check other queens
    (safe-iter 1 (cdr positions))))

(safe? 3 (list 2 2 0))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map
              (lambda (new-row) (adjoin-position new-row k rest-of-queens))
              (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(length (queens 8))