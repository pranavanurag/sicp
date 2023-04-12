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

(define (successive-merge sorted-leaves)
  (cond
    ((null? sorted-leaves) '())
    ((null? (cdr sorted-leaves)) (car sorted-leaves))
    ((null? (cddr sorted-leaves)) (make-code-tree (car sorted-leaves) (cadr sorted-leaves)))
    (else
      (successive-merge
        (adjoin-set
          (make-code-tree (car sorted-leaves) (cadr sorted-leaves))
          (cddr sorted-leaves))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define song-pairs
  (list 
    (list 'A 2)
    (list 'GET 2)
    (list 'SHA 3)
    (list 'WAH 1)
    (list 'BOOM 1)
    (list 'JOB 2)
    (list 'NA 16)
    (list 'YIP 9)))

(define song-htree (generate-huffman-tree song-pairs))

(define song-message (append
  (list 'Get 'a 'job)
  (append
    (list 'Sha 'na 'na 'na 'na 'na 'na 'na 'na)
    (append
      (list 'Get 'a 'job)
      (append
        (list 'Sha 'na 'na 'na 'na 'na 'na 'na 'na)
        (append
          (list 'Wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip)
          (list 'Sha 'boom)))))))

(length (encode song-message song-htree))
(length song-message)
;there are 8 unique symbols
;using a fixed length encoding, we would need 3 bits to store a symbol
;number of symbols in the song = 36
;it would require 36 * 8 = 288 bits
;we used 84 bits using the huffman encoding


(* 36 8)