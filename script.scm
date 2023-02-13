(define x (cons (list 1 2) (list 3 4)))

(length x)

(define (count-leaves tree)
  (cond ((null? tree) 0)
    ((not (pair? tree)) 1)
    (else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))

(count-leaves x)

(define mytree (list 1 (list 2 (list 3 4))))
(car mytree)
(cdr mytree)
(car (cdr mytree))
(cdr (cdr mytree))


(define mylist (list 1 2))
(car mylist)
(cdr mylist)

(define mypair (cons 1 2))
(car mypair)
(cdr mypair)