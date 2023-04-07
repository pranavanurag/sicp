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


(define (adjoin-set x set)
  (cond
    ((null? set) (make-leaf x))
    ((= x (entry set)) set)
    ((> x (entry set))
      (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))
    ((< x (entry set))
      (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))))


(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append
      (tree->list-1 (left-branch tree))
      (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
          (left-branch tree)
          (cons
            (entry tree)
            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))


(define rep1
  (make-tree
    7
    (make-tree 3 (make-leaf 1) (make-leaf 5))
    (make-tree 9 '() (make-leaf 11))))

(define rep2
  (make-tree
    3
    (make-leaf 1)
    (make-tree
      7
      (make-leaf 5)
      (make-tree 9 '() (make-leaf 11)))))


(define rep3
  (make-tree
    5
    (make-tree 3 (make-leaf 1) '())
    (make-tree 9 (make-leaf 7) (make-leaf 11))))

rep1
rep2
rep3

(tree->list-1 rep1)
(tree->list-1 rep2)
(tree->list-1 rep3)

(tree->list-2 rep1)
(tree->list-2 rep2)
(tree->list-2 rep3)

(define (qoutient dividend divisor)
  (/ (- dividend (remainder dividend divisor)) divisor))

(define (partial-tree elts n)
  ;(newline) (display "partial-tree invoked with elts = ") (display elts) (display ", n = ") (display n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (qoutient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let
          ((left-tree (car left-result))
            (non-left-elts (cdr left-result))
            (right-size (- n (+ left-size 1))))
          (let
            ((this-entry (car non-left-elts))
              (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let
              ((right-tree (car right-result))
                (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))


(list->tree (list 1 2 4 5 6))
(list->tree (list 1 3 5 7 9 11))

(define (union-ordered-list set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
      (let ((x1 (car set1)) (x2 (car set2)))
       (cond
        ((= x1 x2) (cons x1 (union-ordered-list (cdr set1) (cdr set2))))
        ((> x1 x2) (cons x2 (union-ordered-list set1 (cdr set2))))
        ((< x1 x2) (cons x1 (union-ordered-list (cdr set1) set2))))))))

(define (intersection-ordered-list set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond
        ((= x1 x2) (cons x1 (intersection-ordered-list (cdr set1) (cdr set2))))
        ((< x1 x2) (intersection-ordered-list (cdr set1) set2))
        ((< x2 x1) (intersection-ordered-list set1 (cdr set2)))))))

(define (union-set-btree t1 t2)
  (let ((l1 (tree->list-2 t1)) (l2 (tree->list-2 t2)))
    (list->tree (union-ordered-list l1 l2))))

(define (intersection-set-btree t1 t2)
  (let ((l1 (tree->list-2 t1)) (l2 (tree->list-2 t2)))
    (list->tree (intersection-ordered-list l1 l2))))

(union-set-btree (list->tree (list 1 3 5 7 9 11)) (list->tree (list 0)))
(intersection-set-btree (list->tree (list 1 3 5 7 9 11)) (list->tree (list 1 3)))