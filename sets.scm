#lang r5rs
(define (error s x) (display "error: ") (display s) (display " ") (display x))

;; ***********************
;; *                     *
;; *  REPRESENTING SETS  *
;; *                     *
;; ***********************
;; ========================
;; Sets as unordered lists.
;; ========================
;(define (element-of-set? x set) ; O(n)
;  (cond ((null? set) false)
;        ((equal? x (car set)) true)
;        (else (element-of-set? x (cdr set)))))
;(define (adjoin-set set x) ; O(n)
;  (if (element-of-set? set x)
;      set
;      (cons x set)))
;(define (intersection-set set1 set2) ; O(n^2)
;  (cond ((or (null? set1) (null? set2)) '())
;        ((element-of-set? (car set1) set2) (cons (car set1)
;                                                (intersection-set (cdr set1) set2)))
;        (else (intersection-set (cdr set1) set2))))
;; [ex 2.59] Union-set ; O(n^2)
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
;        (else (cons (car set1)
;                    (union-set (cdr set1) set2)))))
;
;; [ex 2.60] Representation with Duplicates.
;; element-of-set? and union-set are the same.
;(define (adjoin-set set x)
;  (cons x set))
;(define (union-set set x)
;  (append s t))
;; adjoin-set and union-set are faster, since they combine elements to a base set without checking whether they're
;; already there. element-of-set? will be about as fast as before for positive results, but slower for negative results
;; due to increased list size. Intersection-set will also be slower as a result.
;; This representation may be useful if you are usually checking set membership when you expect a positive result,
;; and are more likely to use union than intersection operations.

;; ======================
;; Sets as ordered lists.
;; ======================
;(define (element-of-set? set x) ; O(n)
;  (cond ((null? set) false)
;        ((= x (car set)) true)
;        ((< x (car set)) false)
;        (else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2) ;O(n)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1
                               (intersection-set (cdr set1)
                                                 (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              ((> x1 x2) (intersection-set set1 (cdr set2)))))))
;; [ex 2.61] adjoin-set ; O(n)
;(define (adjoin-set set x)
;  (cond ((null? set) (list x))
;        ((= x (car set)) set)
;        ((< x (car set)) (cons x set))
;        ((> x (car set)) (adjoin-set x (cdr set)))))
; [ex 2.62] union-set ; O(n)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2) (cons x1
                                       (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1
                                       (union-set (cdr set1) set2)))
                      ((> x1 x2) (cons x2
                                       (union-set set1 (cdr set2)))))))))

;; =====================
;; Sets as binary trees.
;; =====================
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set) ; O(log n)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))
(define (adjoin-set x set) ; O(log n)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))
; The logarithmic growth of these procs rests on the fact that a tree will be "balanced"
; on average, ie. the left and right branches will be the same size.
; However, this is not a guarantee, and it is possible for very simple circumstances to
; result in highly unbalanced trees. One solution is to implement "self-balancing trees".

; procs for converting between lists and trees
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
(define tree->list tree->list-1)
; [ex 2.63 a] Compare the procedures above.
;(define tree1
;  (make-tree 7
;             (make-tree 3 '(1 () ()) '(5 () ()))
;             (make-tree 9 '() '(11 () ()))))
;(define tree2
;  (make-tree 3
;             (make-tree 1 '() '())
;             (make-tree 7
;                        '(1 () ())
;                        (make-tree 9 '() '(11 () ())))))
;(define tree3
;  (make-tree 5
;             (make-tree 3 '(1 () ()) '())
;             (make-tree 9 '(7 () ()) '(11 () ()))))
; The 2 procs give identical results for these trees.
; They appear to produce identical results.
; [ex 2.63 b] Compare the order of growth in the number of steps
;             required to convert a balanced list.
;  Both appear to have O(n) growth.

; [ex 2.64 a]
; list->tree - proc for converting an ordered list to a balanced binary tree.
; partial-tree is a helper proc which takes an ordered list and a number n (<= list len)
; and returns (as a pair) a balanced tree of the first n elements, and the
; remainder of the ordered list.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n) ; make a tree of first n elements, leave the rest in a list
  (if (= n 0)
      (cons '() elts) ; if n is zero, tree is empty, entire list is "remainder"
      (let ((left-size (quotient (- n 1) 2))) ; of n elts to convert, 1 is parent. Half the remainder form the left-tree
        (let ((left-result (partial-tree elts left-size))) ; recursive call to form left-tree (+ remainder-"non-left")
          (let ((left-tree (car left-result)) ; left-tree is first element of the result pair.
                (non-left-elts (cdr left-result)) ; "non-left" remainder is second element of result pair.
                (right-size (- n (+ left-size 1)))) ; right-size is n-left-size-1(for parent) -remaining to convert
            (let ((this-entry (car non-left-elts)) ; first in "non-left" is the central "parent" element
                  (right-result (partial-tree (cdr non-left-elts) ; rest of "non-left" list will form the right tree plus
                                              right-size)))       ; remainder list; use a recursive call to partial-tree
              (let ((right-tree (car right-result)) ; right-result is a cons of right-tree and the remainder list
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) ; construct the tree from parent and child-trees...
                      remaining-elts))))))))                      ; and cons with remaining elements.
; To build a binary tree we can work left->right, forming elements into trees and supertrees. We only need to know
; the total number of elements to calulate the dimensions of the tree. This is dome most straightforwardly using
; recursion - and this is what list->tree/partial-tree do. At each recursive step, the list is divided into a central
; element, left-region and right-region and elements are converted from left to right.
; [ex 2.64 b] The order or list->tree is O(n).

; [ex 2.65]
(define (union-set-tree set1 set2)
  (list->tree (union-set (tree->list set1)
                         (tree->list set2))))
(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set (tree->list set1)
                                (tree->list set2))))

;; ===============================
;; Sets and information retrieval.
;; ===============================
; [ex 2.66] Implement lookup proc for the case where the set of records is structured as a binary tree (ordered by key).
(define (make-record key value) (cons key value))  ; simple test implementation of records.
(define (key record) (car record))
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((root-entry (entry set-of-records)))
        (cond ((= given-key (key root-entry)) root-entry)
              ((< given-key (key root-entry)) (lookup given-key (left-branch set-of-records)))
              (else (lookup given-key (right-branch set-of-records)))))))


;; **************************************
;; *                                    *
;; *  EXAMPLE: HUFFMAN ENCODING TREES.  *
;; *                                    *
;; **************************************
;; leaf node constructor & selectors
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
;; Huffman tree constructor & selectors
(define (make-huffman left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-huffman tree) (car tree))
(define (right-huffman tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
;; decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-huffman branch))
        ((= bit 1) (right-huffman branch))
        (else (error "bad bit -- CHOOSE BRANCH" bit))))
;; encoding procedure
;; ------------------
;; we will represent a set of leaves and trees as a list of elements,
;; ordered by increasing weight
(define (huffman-adjoin x set)  ; assume x is not in set; insert by weight
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (huffman-adjoin x (cdr set))))))
;; proc to convert a list of symbol-frequency pairs to an initial ordered
;; set of leaves, ready to be merged. NOTE: "pairs" are 2-lists, not cons pairs
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((first-pair (car pairs)))
        (huffman-adjoin (make-leaf (car first-pair)
                                   (cadr first-pair))
                        (make-leaf-set (cdr pairs))))))
;; [ex 2.67]
(define sample-tree
  (make-huffman (make-leaf 'A 4)
                (make-huffman (make-leaf 'B 2)
                              (make-huffman (make-leaf 'D 1)
                                            (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; (decode sample-message sample-tree)  ; {a d a b b c a}
;; [ex 2.68]
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol sym tree)
  (define (iter branch result)
    (if (leaf? branch)
        result
        (let ((left (left-huffman branch))
              (right (right-huffman branch)))
          (cond ((memq sym (symbols left)) (iter left
                                              (append result (list 0))))
                ((memq sym (symbols right)) (iter right
                                               (append result (list 1))))
                (else (error "symbol not present in branch" branch))))))
  (iter tree '()))
;; [ex 2.69] complete the encoding proc by writing proc successive-merge
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
;; successive-merge uses make-huffman to successively merge the least-weight
;; elements of the set until there is a single element, the Huffman tree.
(define (successive-merge hufflist)  ; hufflist is an ordered list, initially of leaves
  (cond ((null? hufflist) (error "no leaves"))
        ((null? (cdr hufflist)) (car hufflist))
        (else
         (let ((first (car hufflist))
               (second (cadr hufflist))
               (rest (cddr hufflist)))
           (successive-merge (huffman-adjoin (make-huffman first second)
                                            rest))))))
;; [ex 2.70]
(define rock-data
  (list (list 'a' 2) (list 'boom' 1) (list 'get' 2) (list 'job' 2)
        (list 'na' 16) (list 'sha' 3) (list 'yip' 9) (list 'wah' 1)))
(define rock-message
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))
;; 84 bits are required for the encoding of the 36-word message.
;; A fixed-length code would require 3*36=108 bits.

;; [ex 2.71] Consider a Huffman tree for an alphabet of n symbols,
;; with relative frequencies 1,2,4,...,2^(n-1)
;; The graph for such a tree joins each element in the list to the subtree
;; consisting of all previous elements (since each element has weight one
;; greater than the sum of all preceding weights).
;; The most frequent symbol requires a single bit to encode.
;; The least frequent symbol requires n bits to encode.

;; (ex 2.72)
;; The number of steps needed to encode the most and least frequent symbol
;; increases linearly in each case. The most frequent symbol will be found
;; last, after searching n elements, but is on a branch of its own. The least
;; frequent symbol is highly nested, so n-1 recursions will be required to
;; return the leaf. The procs will have to search through n-1 elements, then
;; n-2, n-3,...,1 element, so the complexity will be O(n^2). If we ensure the
;; less frequent element is always on the left branch of a Huffman tree, we
;; should be able to reduce this to O(n), since the least frequent symbol will
;; always be found first. This is actually the case with my implementation of
;; successive-merge, but is not required by the specification.
