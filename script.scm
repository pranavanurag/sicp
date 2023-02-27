(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op
      (car sequence)
      (accumulate op initial (cdr sequence)))))

(define (count-leaves x)
  (cond
    ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(define (flatten-tree x)
  (newline) (display "flatten-tree invoked with: ") (display x)
  (cond
    ((null? x) '())
    ((not (pair? x)) (list x))
    (else (append (flatten-tree (car x)) (flatten-tree (cdr x))))))

(define t1 (list 1 (list 2 3)))
(define t2 (list 1 (list 2 3) (list 4 5 (list 6 (list 7) 8))))

(count-leaves t1)

(map flatten-tree t2)

(count-leaves t2)


(pair? (list 7))
(car (list 7))
(cdr (list 7))

(append (list 7) '())


(define (count-leaves-2 x)
  (newline) (display "count-leaves-2 invoked with: ") (display x)
  (accumulate
    +
    0
    (map
      (lambda (node)
        (cond
          ((null? node) 0)
          ((not (pair? node)) 1)
          (else (count-leaves-2 node))))
      x)))

(count-leaves-2 t2)