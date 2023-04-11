(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))



(define (left-branch node) (car node))
(define (right-branch node) (cadr node))
(define (symbols node)
  (if (leaf? node) (list (symbol-leaf node)) (caddr node)))
(define (weight node)
  (if (leaf? node) (weight-leaf node) (cadddr node)))
(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))



(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let
        ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
        (adjoin-set
          (make-leaf (car pair) (cadr pair))
          (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree (make-leaf 'B 2) (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))

(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((eq? (car set) x) true)
    (else (element-of-set? x (cdr set)))))


(define (encode-symbol sym tree)
  ;(newline) (display "encode-symbol invoked with: ") (display sym) (display " ") (display tree)
  (cond
    ((leaf? tree)
      (if (eq? (symbol-leaf tree) sym) '() (error "encode-symbol unknown symbol: " sym)))
    ((element-of-set? sym (symbols (left-branch tree)))
      (cons '0 (encode-symbol sym (left-branch tree))))
    ((element-of-set? sym (symbols (right-branch tree)))
      (cons '1 (encode-symbol sym (right-branch tree))))))


(define (encode message tree)
  ;(newline) (display "encode invoked with ") (display message) (display tree)
  (if (null? message)
    '()
    (append
      (encode-symbol (car message) tree)
      (encode (cdr message) tree))))


(encode '(A B C) sample-tree)

(decode '(0 1 0 1 1 1) sample-tree)