(define (square-tree tree)
  (map (lambda (subtree)
    (if (pair? subtree) (square-tree subtree) (* subtree subtree)))
    tree))

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;(display x)
(square-tree x)