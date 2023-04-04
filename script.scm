(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-leaf value) (make-tree value '() '()))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (newline) (display "element-of-set? invoked with x = ") (display x) (display ", and set = ") (display set)
  (cond
    ((null? set) false)
    ((= x (entry set)) true)
    ((< x (entry set)) (element-of-set? x (left-branch set)))
    ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define mytree
  (make-tree
    7
    (make-tree 3 (make-leaf 1) (make-leaf 5))
    (make-tree 9 '() (make-leaf 11))))

(display mytree)
(element-of-set? 5 mytree)