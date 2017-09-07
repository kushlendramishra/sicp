#lang sicp

;; General procedures

(define (square x) 
  (* x x))

(define (exp-iter b n)
  (define (iter base accu pow)
    (cond ((= pow 1) (* accu base))
          ((even? pow) (iter (* base base) accu (/ pow 2)))
          (else (iter base (* accu base) (- pow 1)))))
  (if (= n 0) 
    1
    (iter b 1 n)))

;;2.1
(define (make-rat1 n d)
   (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

(define (make-rat n d)
  (define g (gcd n d))
  (let ((rn (/ (abs n) g))
        (rd (/ (abs d) g)))
    (newline)
    (display rn)
    (newline)
    (display rd)
    (if (xor (< n 0) (< d 0))
      (cons (- rn) rd)
      (cons  rn rd))))


;;2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (length-segment s)
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
    (sqrt (+ (square (- x2 x1))
             (square (- y2 y1))))))


(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
        (y1 (y-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y2 (y-point (end-segment s))))
    (make-point (/ (+ x1 x2) 2.0)
                (/ (+ y1 y2) 2.0))))

(define p1 (make-point 6 4))
(define p2 (make-point 10 16))
(define s1 (make-segment p1 p2))
(print-point (midpoint-segment s1))


;;2.3
(define (rectangle-perimeter rect)
  (let ((s1 (rect-segment rect 1))
        (s2 (rect-segment rect 2)))
    (* 2 (+ (length-segment s1)
            (length-segment s2)))))


(define (rectangle-area rect)
  (let ((s1 (rect-segment rect 1))
        (s2 (rect-segment rect 2)))
    (*  (length-segment s1)
        (length-segment s2))))

;;2.4

(define (cons1 x y)
  (lambda (m) (m x y)))
(define (car1 z)
  (z (lambda (p q) p)))
(define (cdr1 z)
  (z (lambda (p q) q)))

;;2.5
(define (repeated-div z d)
  (define (iter dividend times)
    (if (= (remainder dividend d) 0)
      (iter (/ dividend d) (+ times 1))
      times))
  (iter z 0))



(define (cons2 a b)
  (* (exp-iter 2 a) 
     (exp-iter 3 b)))

(define (car2 z)
  (repeated-div z 2))

(define (cdr2 z)
  (repeated-div z 3))

(define intp (cons2 4 3))

;;(car2 intp)
;;(cdr2 intp)

;;2.6
(define one (lambda (f) 
              (lambda (x) (f x))))

;; 2.7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(define (make-interval a b) (cons a b))

(define (get-bound f x)
  (let ((a (car x))
        (b (cdr x)))
    (f a b)))

(define (lower-bound x)
  (get-bound min x))

(define (upper-bound x)
  (get-bound max x))
 
(define (sub-interval x y)
  (- (lower-bound x) (lower-bound y))
  (- (upper-bound x) (upper-bound y)))

;; 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ (* c p) 100)))
    (make-center-width c w)))

(define (percent i) 
  (let ((w (width i))
        (c (center i)))
    (* (/ w c) 100)))

;;2.17

(define (last-pair l)
  (if (null? (cdr l))
    (list (car l))
    (last-pair (cdr l))))

(last-pair (list 1 2 3))

(null? (list))

(reverse (list 1 2 3 4))
;;2.18

(define (reverse1 l) 
  (if (null? l)
    l
    (append (reverse1 (cdr l)) 
            (list (car l)))))

;;(reverse1 (list 1 2 4 5))

;;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

;;(cc 100 us-coins)
;;(cc 100 (reverse us-coins))

;;2.20

(define (same-parity x . y)
  (define first-parity 
    (if (even? x)
      even?
      odd?))
  (define (iter rlist alist)
    (cond ((null? alist) rlist)
          (else (if (first-parity (car alist))
                  (iter (append rlist (list (car alist)))
                        (cdr alist))
                  (iter rlist (cdr alist))))))
  (iter (list x) y))
                          

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;;2.21
(define (square-list1 items)
  (if (null? items)
    nil
    (cons (square (car items)) 
          (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

;;2.23 
(define (for-each1 f items)
  (if (null? items)
    true
    (and (= 1 (f (car items)))
         (freach f (cdr items)))))

;;2.24
(list 1 (list 2 (list 3 4)))

;;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;;2.27
(define (reverse1 l) 
  (if (null? l)
    l
    (append (reverse1 (cdr l)) 
            (list (car l)))))

(define (deep-reverse x)
  (cond ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x)) 
                      (list (deep-reverse (car x)))))))
                               
(define x (list (list 1 2 (list 5 6)) (list 3 (list 8 9) 4)))

x
(reverse1 x)

(deep-reverse x)

;;2.28
(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

(fringe x)

;;2.29
;;(define (make-mobile left right)
;;  (list left right))
;;
;;(define (make-branch length structure)
;;  (list length structure))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch x)
  (car x))

(define (right-branch x)
  (car (cdr x)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (if (pair? (cdr b))
      (cdr b))
      (car (cdr b)))

(define (branch-weight b)
  (let ((bs (branch-structure b)))
    (if (pair? bs)
      (total-weight bs)
      bs)))

(define (total-weight m)
  (let ((leftb (left-branch m))
        (rightb (right-branch m)))
    (+ (branch-weight leftb) (branch-weight rightb))))

(define (branch-balanced? b)
  (let ((bs (branch-structure b)))
    (if (pair? bs)
      (balanced? bs)
      true)))



(define (balanced? m)
  (display m)
  (newline)
  (let ((leftb (left-branch m))
        (rightb (right-branch m)))
    (let ((leftl (branch-length leftb))
          (rightl (branch-length rightb))
          (leftw  (branch-weight leftb))
          (rightw (branch-weight rightb))
          (leftbalance (branch-balanced? leftb))
          (rightbalance (branch-balanced? rightb)))
      (and leftbalance
           rightbalance
           (= (* leftl leftw)
              (* rightl rightw))))))
        

;;(define b1 (make-branch 2 3))
;;(define b2 (make-branch 2 4))
;;(define b3 (make-branch 2 6))
;;(define b4 (make-branch 2 6))
;;(define b5 (make-branch 2 6))
;;
;;(define m1 (make-mobile b1 b2))
;;(define b6 (make-branch 2 m1))
;;(define m2 (make-mobile b3 b6))
;;(define m3 (make-mobile b4 b5)) 
;;(define b7 (make-branch 2 m2))
;;(define b8 (make-branch 2 m3))
;;(define m4 (make-mobile b7 b8))
;;
;;
;;(total-weight m1)
;;
;;(total-weight m2)
;;
;;(total-weight m3)
;;
;;(total-weight m4)
;;
;;(balanced? m1)
;;
;;(balanced? m2)
;;
;;(balanced? m3)
;;
;;(balanced? m4)

;;2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree2 tree)
  (map (lambda (sub-tree) 
         (if (pair? sub-tree)
           (square-tree2 sub-tree)
           (square sub-tree)))
       tree))

;;2.31
(define (tree-map f tree) 
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

(define (square-tree3 tree) (tree-map square tree))
;;(define (square-tree3 tree) (tree-map tree square))

;;(square-tree test-tree)
;;(square-tree2 test-tree)
;;(square-tree3 test-tree)

;;2.32
;; This works because set of subsets of n elements is the set of subsets of (n-1) elements unioned with the set having the nth element added to each of the subsets.
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define testl (list 1 2 3 4 5))

;;(display (subsets testl))

;;2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (list-id sequence) 
  (accumulate (lambda (x y) (cons x y)) nil sequence))

(list-id (list 1 2 3 4 (list 3  4 5)))

(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define tlist (list 1 2 3 4 5 6 7 8 9 10))

(display (map1 square tlist))

(define tlist2 (list 6 7 8 9))

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))

(display (append1 tlist tlist2))

(define (length1 sequence) 
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(display (length1 tlist))
(display (length1 tlist2))

;;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;;2.35
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                       (enumerate-tree (cdr tree))))))



(define (count-leaves t)
  (accumulate + 0 
              (map length1 (map enumerate-tree t))))

(display test-tree)
(newline)
(display (enumerate-tree test-tree))
(newline)
(display (map enumerate-tree test-tree))
(newline)

(count-leaves test-tree)

;;2.36
(define (accumulate-n op init seqs)
   (if (null? (car seqs))
     nil
     (cons (accumulate op init (map car seqs))
           (accumulate-n op init (map cdr seqs)))))

;;2.37

(define v1 (list 1 2 3))
(define v2 (list 4 5 6))
(define v3 (list 7 8 9))

(define v4 (list 10 11 12))
(define v5 (list 13 14 15))
(define v6 (list 16 17 18))

(define m1 (list v1 v2 v3))
(define m2 (list v4 v5 v6))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product v1 v2)

(define x1 (list 2 2 2))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(matrix-*-vector m1 x1)

(define (transpose mat) 
  (accumulate-n cons nil mat))

(display (transpose m1))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map 
      (lambda (v) (matrix-*-vector cols v))  
      m)))



(display (matrix-*-matrix m1 m2))

;;2.38
(define (fold-left op initial sequence)
   (define (iter result rest)
     (if (null? rest)
       result
       (iter (op result (car rest))
             (cdr rest))))
   (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(display (fold-right list nil (list 1 2 3)))
(display (fold-left list nil (list 1 2 3)))

;; op should be a commutative operation so that fold-left and fold-right give the same result

;;2.39

(define (reverse2 sequence)
   (fold-right (lambda (x y) (append y (list x))) nil sequence))

(display (reverse2 v1))
(newline)

(define (reverse3 sequence)
   (fold-left (lambda (x y) (cons y x)) nil sequence))

(display (reverse3 v1))
(newline)

(display (append 3 (list nil)))

;;2.40

;;(define (enumerate-interval m n)
;;  (define (iter k enum-list)
;;    (if (> k n) 
;;      enum-list
;;      (iter (+ k 1) (append enum-list (list k)))))
;;  (iter m nil))

(define (filter1 predicate items) 
  (cond ((null? items) items)
        ((predicate (car items))
         (cons (car items) (filter1 predicate (cdr items))))
        (else (filter1 predicate (cdr items)))))
          


(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(display (enumerate-interval 3 10))

(define (flat-map proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n) 
  (flat-map (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))


(display (unique-pairs 5))

(define (prime-sum? pair)
   (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
   (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter prime-sum?
               (unique-pairs n))))

(display (prime-sum-pairs 5))

;;2.41
(define (triplets-from-pairs i pairs)
  (map (lambda (inpair) (append inpair (list i)))
            pairs))

(define (unique-triplets n)
  (flat-map (lambda (i) (triplets-from-pairs i (unique-pairs (- i 1))))
            (enumerate-interval 1 n)))

(display (unique-triplets 5))

(define (sum-to s triplet)
  (= s
     (accumulate + 0 triplet)))

(define (sum-to-triplets s n)
  (filter1 (lambda (t) (sum-to s t))
          (unique-triplets n)))

(display (sum-to-triplets 7 5))

(display (list 1 2 3))
(display (unique-triplets 5))

;;2.42
(define empty-board nil)

(define (adjoin-position row col rest-of-queens)
  (append rest-of-queens (list row col)))

(define (safe-queen-pair pos1 pos2)
  (let ((row1 (car pos1))
        (col1 (cadr pos1))
        (row2 (car pos2))
        (col2 (cadr pos2)))
    (and (not (= row1 row2))
         (not (= (abs (- row1 row2))
                 (abs (- col1 col2)))))))


(define (get-item s k)
  (if (= k 0)
    (cadr s)
    (get-item (cdr k) (- k 1))))

(define (safe? k posistions)
  (let ((kpos (get-item s k)))
    (accumulate (lambda (safety newpose)
                  (and safety
                       (safe-queen-pair kpos newpose)))
                positions)))



(define (queens board-size)
   (define (queen-cols k)
     (if (= k 0)
       (list empty-board)
       (filter
         (lambda (positions) (safe? k positions))
         (flatmap
           (lambda (rest-of-queens)
             (map (lambda (new-row)
                    (adjoin-position new-row k rest-of-queens))
                  (enumerate-interval 1 board-size)))
           (queen-cols (- k 1))))))
   (queen-cols board-size))


;;2.53

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


(list 'a 'b 'c)

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))

;;2.54

(define (equal1? a b)
  (let ((pa (pair? a))
        (pb (pair? b)))
    (cond ((or (not pa) (not pb))
           (eq? a b))
          (else (and (eq? (car a) (car b))
                     (eq? (cdr a) (cdr b)))))))

(equal1? '(a b (c d) e) '((a b) c d e))

;;2.55
;;it seems like a string of quated characters is interpreted as a quated list. the second quote is also a part of the string.
(car ''abracadabra)


;;2.56
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
   (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list 'a1 + a2))))


(define (=number? a b)
  (and (number? a) (= a b)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list 'm1 * m2))))

(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (expt e1 e2))
        (else (list 'e1 ** e2))))

(define (sum? x)
   (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
   (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (exponent? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponent? exp)
         (make-product 
           (make-product (exponent exp) 
                         (make-exponentiation (base exp) (- (exponent exp) 1)))
           (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))


(display (deriv '(+ x 3) 'x))


(display (deriv '(* x y) 'x))


(display (deriv '(* (* x y) (+ x 3)) 'x))

(display (deriv '(** x 5) 'x))

(display (make-exponentiation 'x 5))

;;2.57


(define (addend s) (cadr s))

(define (augend s) 
  (let ((term (cddr s)))
    (if (null? (cdr term))
     (car term)
     (cons '+ term))))

;;(define (multiplier p) (cadr p))

(define (multiplicand p) 
  (let ((term (cddr p)))
    (if (null? (cdr term))
     (car term)
     (cons '* term))))

(display (deriv '(*  x y (+ x 3)) 'x))

;;2.58 a
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2) (list '* m1 m2))

(define (=number? a b)
  (and (number? a) (= a b)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation e1 e2)
  (cond ((=number? e2 0) 1)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (expt e1 e2))
        (else (list '** e1 e2))))

(define (sum? x)
   (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
   (and (pair? x) (eq? (car x) '*)))

;;2.58 b TODO

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;2.59
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

;;2.60
;;no definitions change except union-set. This implementation of sets would be suitable for applications where there are relatively few unique set elements and storage is available in plenty

(define (union-set2 set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

(define myset1 '(1 2 3 4))
(define myset2 '(3 4 5 6 7 8))

(define myset1 '())
(define myset2 '(3 4 5 6 7 8))

(define myset1 '(1 2 3 4))
(define myset2 '())

(define myset1 '(1 2 3 4))
(define myset2 '(1 2 3 4))

(define myset1 '(1 2 3 4))
(define myset2 '(5 6 7 8))

(intersection-set myset1 myset2)
(union-set myset1 myset2)

;;2.61 to me it seems there should be no change in adjoin-set, only the element-of-set that function calls should be the ordered version

;;2.62
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set3 set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                  (cons x1
                         (intersection-set3 (cdr set1)
                                            (cdr set2))))
                 ((< x1 x2)
                   (intersection-set3 (cdr set1) set2))
                  ((< x2 x1)
                    (intersection-set3 set1 (cdr set2)))))))

(define (union-set3 set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set3 (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set3 (cdr set1) set2)))
                      ((> x1 x2) (cons x2 (union-set3 set1 (cdr set2)))))))))


(define myset1 '(1 2 3 4))
(define myset2 '(3 4 5 6 7 8))

(define myset1 '())
(define myset2 '(3 4 5 6 7 8))

(define myset1 '(1 2 3 4))
(define myset2 '())

(define myset1 '(1 2 3 4))
(define myset2 '(1 2 3 4))

(define myset1 '(1 2 3 4))
(define myset2 '(5 6 7 8))

(intersection-set myset1 myset2)

(union-set3 myset1 myset2)


;;2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


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



;;first tree
;; define trees of figure 2.16
(define t1 
  (make-tree 7
             (make-tree 3 (make-tree 1 '() '()) 
                          (make-tree 5 '() '()))
             (make-tree 9 '() (make-tree 11 '() '()))))

(define t2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7 
                        (make-tree 5 '() '())
                        (make-tree 9 '() (make-tree 11 '() '())))))

(define t3
  (make-tree 5
             (make-tree 3 
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))

(tree->list-1 t1)
(tree->list-2 t1)

(tree->list-1 t2)
(tree->list-2 t2)

(tree->list-1 t3)
(tree->list-2 t3)

;; to me it looks like they both have linear order of growth

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; 2.63 a
;; This procedure works by dividing the list approximately two halves. The procedure is applied recursively to each half, and the two resulting balanced binary trees are made left and right subtree of the root node. The root node has an element that is not present in either left or right subtree. The order of growth of this procedure is theta(n) since there is one unique recursive call for each element of the list

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

(display (list->tree (list 1 3 5 7 9 1 11)))

;;2.65
;; Since all the operations in the procedures are theta(n), the procedures are theta(n)

(define (union-set4 tree1 tree2)
  (let ((tree-list1 (tree->list-1 tree1))
        (tree-list2 (tree->list-2 tree2)))
    (list->tree (union-set3 tree-list1 tree-list2))))

(define (intersection-set4 tree1 tree2)
  (let ((tree-list1 (tree->list-1 tree1))
        (tree-list2 (tree->list-2 tree2)))
    (list->tree (intersection-set3 tree-list1 tree-list2))))

(display (union-set4 t1 t2))

(display (intersection-set4 t1 t2))

;;2.66
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (lookup-bt given-key set-of-records)
  (let ((node-key (key (car set-of-records))))
    (cond ((null? set-of-records) false)
          ((= given-key node-key) (car set-of-records))
          ((< given-key node-key) 
           (lookup-bt given-key (left-branch set-of-records)))
          ((> given-key node-key) 
           (lookup-bt given-key (right-branch set-of-records))))))

;;2.67
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-message sample-tree))
;;decoded message (ADABBCA)

;;2.68

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))


(define (encode-symbol symb tree)
  (define (encode-1 bits current-branch)
    (if (leaf? current-branch)
      bits
      (let ((left-branch-symbols (symbols (left-branch current-branch)))
            (right-branch-symbols (symbols (right-branch current-branch))))
        (cond ((element-of-set? symb left-branch-symbols)
               (encode-1 (append bits (list '0)) (left-branch current-branch)))
              ((element-of-set? symb right-branch-symbols)
               (encode-1 (append bits (list '1)) (right-branch current-branch)))
              (else (error "bad symbol -- ENCODE-SYMBOL" bit))))))
  (encode-1 '() tree))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define sample-message2 '(A D A B B C A))

(display (encode sample-message2 sample-tree))

;;2.69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
   (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? leaf-set) 
    '()
    (if (= 1 (length leaf-set))
      (car leaf-set)
      (let ((first-pair (car leaf-set))
            (second-pair (cadr leaf-set))
            (remaining-pairs (cddr leaf-set)))
        (let ((merged-pair (make-code-tree first-pair second-pair)))
          (successive-merge (adjoin-set merged-pair remaining-pairs)))))))

;;2.70
(define song-pairs (list (list 'A 2)
                         (list 'NA 16)
                         (list 'BOOM 1)
                         (list 'SHA 3)
                         (list 'GET 2)
                         (list 'YIP 9)
                         (list 'JOB 2)
                         (list 'WAH 1)))

(define song-tree (generate-huffman-tree song-pairs))

(display song-tree)

(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(display (length song))

(define song-code (encode song song-tree))

(display (length song-code))

(display (decode song-code song-tree))

;;84 bits are required to encode the song. Since there are eight symbols, 3 bit would be required to code each symbol. There are 36 words in the song, so total bits required would be 108.

;;2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;2.73 a. Because unlike operators which have commonly understood fixed symbols, there are infinite number of symbols for numbers and different variabls. It would not be possible 'install' an operation for all numbers and variables.

(define (install-deriv-package)
  ;;internal procedures
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list 'a1 + a2))))
  (define (=number? a b)
    (and (number? a) (= a b)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list 'm1 * m2))))
  (define (make-exponentiation e1 e2)
    (cond ((=number? e2 0) 1)
          ((=number? e2 1) e1)
          ((and (number? e1) (number? e2)) (expt e1 e2))
          (else (list 'e1 ** e2))))
  (define (sum? x)
    (and (pair? x) (eq? (cadr x) '+)))
  (define (addend s) 
    (display "ADDEND")
    (newline)
    (car s))
  (define (augend s) 
    (display "AUGEND")
    (newline)
    (caddr s))
  (define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))
  (define (multiplier p) (car p))
  (define (multiplicand p) (caddr p))
  (define (first-operand ops) 
    (car ops))
  (define (second-operand ops) 
    (cadr ops))
  (define (exponent? x)
    (and (pair? x) (eq? (cadr x) '**)))
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))
  ;;interface to the rest of the system
  (put 'deriv '+ 
       (lambda (operands var) 
         (display operands)
         (newline)
         (display "HERE")
         (newline)
         (make-sum (deriv (first-operand operands) var)
                   (deriv (second-operand operands) var))))
  (put 'derive '*
       (lambda (operands var)
         (let ((m1 (first-operand operands))
               (m2 (second-operand operands)))
           (make-sum 
             (make-product m1 (deriv m2 var))
             (make-product (derive m1 var) m2)))))
  ;;2.73 d
  (put 'deriv '** 
       (lambda (operands var)
         (let ((e1 (first-operand operands))
               (e2 (second-operand operands)))
           (make-product e2
                         (make-exponentiation e1 (- e2 1))
                         (deriv exp1))))))


(install-deriv-package)

(display (deriv '(+ x 3) 'x))

(display (deriv '(+ x 3) 'x))

(display *op-table*)



;;2.74

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

;;2.74 a
;;We assume that a package would be implemented for each personnel file
;;which will provide atleast one procedure to search and return an employee
;;record. We also assume that the returned record would be organised as a set of pairs,
;;each pair consisting of a tag identifying the information and the value. Note that
;;each package may store employee info differently, but get-record will return the information
;;organised in a way just described.

(define (get-record emp-name record-file)
  ((get ('get-record 'record-file)) emp-name))

;;2.74 b
(define (get-salary emp-record)
  (if (null? emp-record)
    (error "Salary info not found in the record.")
    (let ((info-pair (car emp-record)))
      (let ((info-tag (get-info-tag info-pair))
            (info-value (get-info-value info-pair)))
        (if (equal? info-tag 'Salary)
          info-value
          (get-salary (cdr emp-record)))))))

;;2.74 c
(define (find-employee-record emp-name record-file-list)
  (if (null? record-file-list)
    (error "Employee record not found.")
    (let ((curr-file (car record-file-list)))
      (let ((emp-record (get-record emp-name curr-file)))
        (if (null? emp-record)
          (find-employee-record emp-name (cdr record-file-list))
          emp-record)))))

;;2.74 d 
;;When Insatiable takes over a new company, a package has to implemented to interface with the acquisition's personnel file. This package needs to provide just one procedure to get an employee record.

;;2.75

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
            (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-angle r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else 
            (error "Unknown op -- MAKE-FROM-MAG-ANGLE" op))))
  dispatch)

;;2.76
;; To me, for both the situations data directed programming seems to be the best solution
;; Operations have to be defined separately for each data type anyway, so adding new operations is equal work for both cases. But data-directed has added advantage that it is better organized and has better additivity.

;;THEGENERIC number package
(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) '()))
;;COERCION
(define *coercion-table* (make-hash))
(define (put-coercion from-type to-type proc)
  (let ((D1 (display (list from-type to-type)))
        (D2 (newline)))
    (hash-set! *coercion-table* (list from-type to-type) proc)))
(define (get-coercion from-type to-type)
  (let ((D1 (display (list from-type to-type)))
        (D2 (newline)))
    (hash-ref *coercion-table* (list from-type to-type) '())))
(define *raise-table* (make-hash))
(define (put-raise from-type proc)
  (let ((D1 (display (list from-type)))
        (D2 (newline)))
    (hash-set! *raise-table* (list from-type) proc)))
(define (get-raise from-type)
  (let ((D1 (display (list from-type)))
        (D2 (newline)))
    (hash-ref *raise-table* (list from-type) '())))

;;2.77
;;SCHEMENUMBERPACKAGE
(define (install-scheme-number-package)
  ;;FOR ex 2.78
  (define (tag x) x)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  ;;FOR ex 2.79
  (put 'equ? '(scheme-number scheme-number) 
       (lambda (x y) (= x y)))
  ;;FOR ex 2.80
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;;FOR ex 2.81 a
  (put 'exp '(scheme-number scheme-number)
        (lambda (x y) (tag (expt x y)))) ; using primitive expt
  ;;FOR ex 2.86
  (put 'square1 '(scheme-number) (lambda (x) (tag (* x x))))
  (put 'sqrt1   '(scheme-number) (lambda (x) (tag (sqrt x))))
  (put 'sin1 '(scheme-number) (lambda (x) (tag (sin x))))
  (put 'cos1 '(scheme-number) (lambda (x) (tag (cos x))))
  (put 'atan '(scheme-number scheme-number)
        (lambda (x y) (tag (atan x y)))) 

  'done)
(define (make-scheme-number n)
   ((get 'make 'scheme-number) n))

;;RATIONALPACKAGE
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                  (* (numer y) (denom x)))
               (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;;FOR 2.85
  (define (project-rat r)
    (quotient (numer r) (denom r)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  ;;FOR ex 2.79
  (put 'equ? '(rational rational)
       (lambda (x y) 
         (let ((nx (numer x))
               (dx (denom x))
               (ny (numer y))
               (dy (denom y)))
           (let ((r1 (* nx dy))
                 (r2 (* ny dx)))
             (= r1 r2)))))
  ;;FOR ex 2.80
  (put '=zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;;FOR ex 2.84
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  ;;FOR ex 2.85
  (put 'project '(rational) project-rat)
  ;;FOR ex 2.86
  (put 'square1 '(rational) 
       (lambda (x)
         (let ((xn (numer x))
               (xd (denom x)))
           (make-rat (* xn xn) (* xd xd)))))
  (put 'sqrt1 '(rational) 
       (lambda (x)
         (let ((xn (numer x))
               (xd (denom x)))
           (sqrt (/ xn xd)))))
  (put 'sin1 '(rational)
       (lambda (x)
         (let ((xn (numer x))
               (xd (denom x)))
           (sin (/ xn xd)))))
  (put 'cos1 '(rational)
       (lambda (x)
         (let ((xn (numer x))
               (xd (denom x)))
           (cos (/ xn xd)))))
  (put 'atan1 '(rational rational)
       (lambda (x y)
         (let ((xn (numer x))
               (xd (denom x))
               (yn (numer y))
               (yd (denom y)))
           (atan (/ xn xd) (/ xn xd)))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;;RECTANGULARPACKAGE
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part1 z) (car z))
  (define (imag-part1 z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude1 z)
    (sqrt1 (add (square1 (real-part1 z))
             (square1 (imag-part1 z)))))
  (define (angle1 z)
    (atan (imag-part1 z) (real-part1 z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos1 a)) (mul r (sin1 a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part1 '(rectangular) real-part1)
  (put 'imag-part1 '(rectangular) imag-part1)
  (put 'magnitude1 '(rectangular) magnitude1)
  (put 'angle1 '(rectangular) angle1)
  ;; FOR ex 2.79
  (put 'equ? '(rectangular rectangular)
       (lambda (z1 z2)
         (let ((x1 (real-part1 z1))
               (y1 (imag-part1 z1))
               (x2 (real-part1 z2))
               (y2 (imag-part1 z2)))
             (and (equ? x1 x2) (equ? y1 y2)))))
  ;; FOR ex 2.80
  (put '=zero? '(rectangular)
       (lambda (z1)
         (and (=zero? (real-part1 z1))
              (=zero? (imag-part1 z1)))))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


;;POLARPACKAGE
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude1 z) (car z))
  (define (angle1 z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part1 z)
    (mul (magnitude1 z) (cos1 (angle1 z))))
  (define (imag-part1 z)
    (mul (magnitude1 z) (sin1 (angle1 z))))
  (define (make-from-real-imag x y)
    (cons (sqrt1 (add (square1 x) (square1 y)))
          (atan1 y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part1 '(polar) real-part1)
  (put 'imag-part1 '(polar) imag-part1)
  (put 'magnitude1 '(polar) magnitude1)
  (put 'angle1 '(polar) angle1)
  ;;FOR ex 2.79
  (put 'equ? '(polar polar)
       (lambda (z1 z2)
         (let ((r1 (magnitude1 z1))
               (a1 (angle1 z1))
               (r2 (magnitude1 z2))
               (a2 (angle1 z2)))
           (and (equ? r1 r2) (equ? a1 a2)))))
  ;; FOR ex 2.80
  (put '=zero? '(polar)
       (lambda (z1)
         (=zero? (magnitude1 z1))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;COMPLEXPACKAGE
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
     (make-from-real-imag (add (real-part1 z1) (real-part1 z2))
                         (add (imag-part1 z1) (imag-part1 z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part1 z1) (real-part1 z2))
                         (sub (imag-part1 z1) (imag-part1 z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude1 z1) (magnitude1 z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
     (make-from-mag-ang (div (magnitude1 z1) (magnitude1 z2))
                         (sub (angle z1) (angle z2))))
   ;; interface to rest of the system
   (put 'real-part1 '(complex) real-part1)
   (put 'imag-part1 '(complex) imag-part1)
   (put 'magnitude1 '(complex) magnitude1)
   ;; FOR 2.85
   (put 'project '(complex) 
        (lambda (z) 
          (make-rational (round (real-part1 z)) 1)))
   (put 'angle1 '(complex) angle1)
   ;; FOR ex 2.79
   ;;(put 'equ? '(complex complex) equ?)
   ;;Redefining for ex 2.85 because the above definition is not general enough.
   ;;It wont work when the two input complex numbers have different representations (rectangular and complex)
   ;; FOR ex 2.85
   (put 'equ? '(complex complex)
        (lambda (z1 z2) 
          (let ((z1-real (real-part1 z1))
                (z1-imag (imag-part1 z1))
                (z2-real (real-part1 z2))
                (z2-imag (imag-part1 z2)))
            (and (equ? z1-real z2-real)
                 (equ? z1-imag z2-imag)))))
   (put '=zero? '(complex) =zero?)
   (define (tag z) (attach-tag 'complex z))
   (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
   (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
   (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
   (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
   (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
   (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
   'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (square x) (* x x))
;; FOR 2.78
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'scheme-number)
    contents
  (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (real-part1 z) (apply-generic 'real-part1 z))
(define (imag-part1 z) (apply-generic 'imag-part1 z))
(define (magnitude1 z) (apply-generic 'magnitude1 z))
(define (angle1 z) (apply-generic 'angle1 z))
(define (exp x y) (apply-generic 'exp x y))

;;TESTING scheme number package
(install-scheme-number-package)

(add 5 7)
(sub 5 7)
(mul 5 7)
(div 5 7)
(equ? 5 7)
(=zero? 7)


;;TESTING rational package
(install-rational-package)

(define rat1 (make-rational 3 5))
(define rat2 (make-rational 4 5))
(define rat3 (make-rational 3 5))
(define rat4 (make-rational 0 5))

(add rat1 rat2)

(sub rat1 rat2)

(mul rat1 rat2)

(div rat1 rat2)

(equ? rat1 rat2)

(equ? rat1 rat3)

(=zero? rat1)

(=zero? rat4)

;;TESTING rectangular package
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define rect1 (make-complex-from-real-imag 3 4))
(define rect2 (make-complex-from-real-imag 5 6))
(define rect3 (make-complex-from-real-imag 3 4))
(define rect4 (make-complex-from-real-imag 0 0))

(display rect1)

(add rect1 rect2)

(sub rect1 rect2)

(mul rect1 rect2)

(div rect1 rect2)

(equ? rect1 rect2)

(equ? rect1 rect3)

(=zero? rect1)

(=zero? rect4)


;;TESTING polar package
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define p1 (make-complex-from-mag-ang 5 (/ pi 4)))
(define p2 (make-complex-from-mag-ang 5 (/ pi 6)))
(define p3 (make-complex-from-mag-ang 5 (/ pi 4)))
(define p4 (make-complex-from-mag-ang 0 (/ pi 4)))


(add p1 p2)

(sub p1 p2)

(mul p1 p2)

(div p1 p2)

(equ? p1 p2)

(equ? p1 p3)

(=zero? p1)

(=zero? rect4)


(define (install-coercion-package)
  ;; procedure to coerce scheme number to complex
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (rational->complex q)
    (let ((qn (numer q))
          (qd (denom q)))
      (scheme-number->complex (/ qn qd))))
  (define (scheme-number->rational n)
    (make-rational n 1))
  ;;FOR ex 2.81
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)
  (define (rational->rational q) q)
  ;; define the interface
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  (put-coercion 'rational 'complex rational->complex)
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  ;;FOR ex 2.81
  ;;(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
  ;;(put-coercion 'rational 'rational rational->rational)
  ;;(put-coercion 'complex 'complex complex->complex)
  'done)

;;The new apply generic wasn't working as it was supposed to. The reason was that '() doesn't evaluate to FALSE as it is intended to in the procedure. This function tests whether '() evaluate to false
(define (test-proc)
  (if '()
    (begin (display "NULL doesn't evaluate to FALSE!")
           (newline))
    (begin (display "NULL evaluates to FALSE."))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if (not (null? proc))
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (not (equal? type1 type2))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond ((not (null? t1->t2))
                       (apply-generic op (t1->t2 a1) a2))
                      ((not (null? t2->t1))
                       (apply-generic op a1 (t2->t1 a2)))
                      (else
                        (error "No method for these types"
                               (list op type-tags)))))
              (error "No method for these types" (list op type-tags)))
            (error "No method for these types"
                   (list op type-tags))))))))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (real-part1 z) (apply-generic 'real-part1 z))
(define (imag-part1 z) (apply-generic 'imag-part1 z))
(define (magnitude1 z) (apply-generic 'magnitude1 z))
(define (angle1 z) (apply-generic 'angle1 z))
(define (exp x y) (apply-generic 'exp x y))

;;2.81
(install-coercion-package)
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


(define rect1 (make-complex-from-real-imag 3 4))
(define rect2 (make-complex-from-real-imag 5 6))
(define rect3 (make-complex-from-real-imag 3 4))
(define rect4 (make-complex-from-real-imag 0 0))


(add rect1 rect2)

(add rect1 5.0)

(add 5.0 rect1)

(exp 3 4)

;;ex 2.81 a. Well, it goes into an infinite loop

(exp rect1 rect2)

;;Well, Louis messed up this time. Having coercions of the same type sends the evaluator into infinite loop if the operation is not defined for the input types.

;;2.82


;;2.83
(define (install-raise-package)
  ;; procedure to coerce scheme number to complex
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (rational->complex q)
    (let ((qn (numer q))
          (qd (denom q)))
      (scheme-number->complex (/ qn qd))))
  (define (scheme-number->rational n)
    (make-rational n 1))
  ;;FOR ex 2.81
  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)
  (define (rational->rational q) q)
  ;; define the interface
  (put-raise 'scheme-number scheme-number->rational)
  (put-raise 'rational rational->complex)
  'done)

;;2.84
(define (raise-type z) 
  (let ((tt (type-tag z)))
    (let ((raise-proc (get-raise tt)))
      (if (not (null? raise-proc))
        (raise-proc z)
        (error "No raise procedure defined for type: " tt)))))

;;2.85
(define (drop-type x) 
  (let ((xtype (type-tag x)))
    (if (equal? xtype 'scheme-number)
      x
      (let ((x-projection (project x)))
        (let ((x-reraise (raise-type x-projection)))
          (if (equ? x x-reraise)
            (drop-type x-projection)
            x))))))


;;2.84
(define (apply-generic op . args)
  (define (type-rank x)
    (let ((type-tag-x (type-tag x)))
      (cond ((equal? type-tag-x 'scheme-number) 0)
            ((equal? type-tag-x 'rational) 1)
            ((equal? type-tag-x 'complex) 2)
            (else 
              (error "Unknown number type")))))
  (define (successive-raise in-args)
    (let ((x (car in-args))
          (y (cadr in-args)))
      (let ((t1 (type-rank x))
            (t2 (type-rank y)))
        (cond ((> t1 t2) 
               (successive-raise (list x (raise-type y))))
              ((< t1 t2)
               (successive-raise (list (raise-type x) y)))
              (else (list x y))))))
  (define (normalize in-args)
    (let ((type-tags (map type-tag in-args)))
      (let ((t1 (car type-tags))
            (t2 (cadr type-tags)))
        (if (equal? t1 t2)
          in-args
          (successive-raise in-args)))))
  (let ((arg-length (length args)))
    (cond ((= arg-length 2)
           (let ((norm-args (normalize args)))
             (let ((type-tags (map type-tag norm-args)))
               (let ((proc (get op type-tags)))
                 (if (not (null? proc))
                   (drop-type (apply proc (map contents norm-args)))
                   (error "No method for these types"
                          (list op type-tags)))))))
          ((= arg-length 1)
             (let ((type-tags (map type-tag args)))
               (let ((proc (get op type-tags)))
                 (if (not (null? proc))
                   (drop-type (apply proc (map contents args)))
                   (error "No method for these types"
                          (list op type-tags))))))
          (else (error "Currently the generic-number package supports unary or binary operations only" )))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))
(define (real-part1 z) (apply-generic 'real-part1 z))
(define (imag-part1 z) (apply-generic 'imag-part1 z))
(define (magnitude1 z) (apply-generic 'magnitude1 z))
(define (angle1 z) (apply-generic 'angle1 z))
(define (exp x y) (apply-generic 'exp x y))
;;FOR ex 2.85
(define (project z) (apply-generic 'project z))


(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-raise-package)

(define r1 (make-rational 3 4))
(define rect1 (make-complex-from-real-imag 3 4))
(define rect2 (make-complex-from-real-imag 5 6))
(define rect3 (make-complex-from-real-imag 3 4))
(define rect4 (make-complex-from-real-imag 0 0))

(add 5.0 r1)

(add rect1 rect2)

(add rect1 5.0)

(add 5.0 3.0)

(add 5.0 rect1)


;;2.85
(define rect1 (make-complex-from-real-imag 3 4))
(define rect2 (make-complex-from-mag-ang 3 4))
(define rect3 (make-complex-from-real-imag 3 0))
(define rect4 (make-complex-from-mag-ang 3 0))

(define rat1 (make-rational 3 4))
(define rat2 (make-rational 3 1))

(drop-type rect1)

(drop-type rect2)

(drop-type rect3)

(drop-type rect4)

(drop-type rat1)
(drop-type rat2)

;;2.86 I think changing the basic operators (+, -, *) to the generic operations (add, sub, mul etc) might be sufficient for this exercise. lets see

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))
(define (real-part1 z) (apply-generic 'real-part1 z))
(define (imag-part1 z) (apply-generic 'imag-part1 z))
(define (magnitude1 z) (apply-generic 'magnitude1 z))
(define (angle1 z) (apply-generic 'angle1 z))
(define (exp x y) (apply-generic 'exp x y))
;;FOR ex 2.85
(define (project z) (apply-generic 'project z))
;;FOR ex 2.86
(define (sin1 x) (apply-generic 'sin1 x))
(define (cos1 x) (apply-generic 'cos1 x))
(define (square1 x) (apply-generic 'square1 x))
