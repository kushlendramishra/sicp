#lang sicp

(define (square x) 
  (* x x))

(define (average a b)
  (/ (+ a b) 2.0))

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

;; Exercise 1.3, define a proc which takes 3 numbers and returns the sum of squares of the larger 2
(define (square-larger-2 x y z)
  (cond ((> x y)
         (if (> y z)
           (sum-of-squares x y)
           (sum-of-squares x z)))
        (else 
          (if (> x z)
           (sum-of-squares x y)
           (sum-of-squares y z)))))

;; tests 1.3
;; (square-larger-2 2 3 4)
;; (square-larger-2 3 4 2)
;; (square-larger-2 2 4 3)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve  guess x) 
               x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve  guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt1 x)
  (sqrt-iter 1.0 x))



;;(sqrt1 9)


;; exercise 1.6, very instructive
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))



(define (sqrt-iter1 guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter1 (improve  guess x) 
               x)))

(define (sqrt2 x)
  (sqrt-iter1 1.0 x))

;; DONT RUN THE TEST BELOW EVER AGAIN, THE COMPUTER HANGS.
;;(sqrt2 9)


;; exercise 1.7
(define (good-enough1? guess next-guess)
  (< (abs (- guess next-guess))
         (/ guess 1000)))


(define (sqrt-iter2 guess x)
  (if (good-enough1? guess (improve guess x)) 
    guess
    (sqrt-iter2 (improve  guess x) 
               x)))

(define (sqrt3 x)
  (sqrt-iter2 1.0 x))

;;(sqrt3 9)
;;(sqrt3 5)

;; exercise 1.8, implement cube-root

(define (iter-cbrt guess x)
  (if (good-enough-cbrt guess (improve-cbrt guess x))
    guess
    (iter-cbrt (improve-cbrt guess x) x)))

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess))
        ( * 2 guess))
     3))

(define (good-enough-cbrt guess next-guess)
  (< (abs (- guess next-guess))
         (/ guess 1000)))

(define (cubert x) 
  (iter-cbrt 1.0 x))

;;(cubert 27)
;;(cubert 8)

;; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;(A 1 10)
;;(A 2 4)
;;(A 3 3)

;;(A 1 1)
;;(A 1 2)
;;(A 1 3)
;;(A 1 4)
;;(A 1 5)

;;exercise 1.11
(define (f1-11 n)
  (define (iter q a b c)
    (if (= q n)
      a
      (iter (+ q 1)
            (+ a (* 2 b) (* 3 c))
            a b)))
  (if (< n 3)
    n
    (iter 2 2 1 0)))
     

;;(f1-11 5)

;;exercise 1.12
(define (pascals row ele)
  (cond ((or (> ele row) (= row 0)) 0)
        ((or (= ele 1) (= ele row)) 1)
        (else (+ (pascals (- row 1) (- ele 1)) 
                 (pascals (- row 1) ele)))))
                 

;;(pascals 15 7)

;;1.16
(define (exp-iter b n)
  (define (iter base accu pow)
    (cond ((= pow 1) (* accu base))
          ((even? pow) (iter (* base base) accu (/ pow 2)))
          (else (iter base (* accu base) (- pow 1)))))
  (if (= n 0) 
    1
    (iter b 1 n)))
(exp-iter 2 0)
(exp-iter 2 1)
(exp-iter 2 2)
(exp-iter 2 3)
(exp-iter 2 4)
(exp-iter 2 5)
(exp-iter 2 6)
(exp-iter 2 7)
(exp-iter 2 8)
(exp-iter 2 9)
(exp-iter 2 13)

;;1.18
(define (double a) 
  (* 2 a))
(define (halve a)
  (/ a 2))

;; recursive definition given in the book
(define (multiply a b)
  (if (= b 0)
    a
    (+ a (multiply a (- b 1)))))

;; iterative definition
(define (iter-mult a b)
  (define (iter m1 accu m2)
    (cond ((= m2 1) (+ m1 accu))
          ((even? m2) (iter (double m1) accu (halve m2)))
          (else (iter m1 (+ accu m1) (- m2 1)))))
  (if (= b 0)
    0
    (iter a 0 b)))

(iter-mult 4 5)

;;1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= 0 count) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p 
                        q
                        (- count 1)))))

;;(fib 10)

;;1.21

(define (square n)
  (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;;(smallest-divisor 19999)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))
    (report-composite)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (report-composite) 
  (newline)
  (display "NOTPRIME"))


;;(timed-prime-test 11)

(define (search-for-primes a b)
  (if (< a b)
    (cond ((even? a) (search-for-primes (+ a 1) b))
          (else ((timed-prime-test a)
                 (search-for-primes (+ a 2) b))))))

;; 1009, 1013, 1019
;;(search-for-primes 1000 1020)

;; 10007, 10009, 10037
;;(search-for-primes 10000 10040)

;; 100003, 100019, 10043
(search-for-primes 100000 100050)

;;1.23
(define (next-divisor a)
  (if (= a 2)
    3
    (+ a 2)))


(define (find-fdivisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-fdivisor n (next-divisor test-divisor)))))

(define (fsmallest-divisor n)
  (find-fdivisor n 2))

(define (timed-prime-test n)
  (newline)
  (display n)
  (fstart-prime-test n (runtime)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (fstart-prime-test n (runtime)))

(define (fstart-prime-test n start-time)
  (if (fprime? n)
    (report-prime (- (runtime) start-time))
    (report-composite)))

(define (fprime? n)
  (= n (fsmallest-divisor n)))

(timed-prime-test 100043)

;;1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else 
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


(define (fast-timed-prime-test n)
  (newline)
  (display n)
  (fast-start-prime-test n (runtime)))

(define (fast-start-prime-test n start-time)
  (if (fast-prime? n 4)
    (report-prime (- (runtime) start-time))
    (report-composite)))

(fast-timed-prime-test 100043)

;; 1.27
(define (fermat-test2 n a)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it a))

(define (thorough-fermat-test n)
  (define (iter a)
    (cond ((= a n) true)
          ((fermat-test2 n a) (fermat-test2 n (+ a 1)))
          (else false)))
  (iter 2))

;;(thorough-fermat-test 561)
;;(thorough-fermat-test 1105)
;;(thorough-fermat-test 1729)
;;(thorough-fermat-test 2465)
;;(thorough-fermat-test 2821)
;;(thorough-fermat-test 6601)

;; 1.28

(define (expmod2 base exp m)
  (define (check-nontrivial-sqrt1 x)
    (and (not (= x 1))
         (= x (- m 1))
         (= (remainder (square x) m) 1)))
  (define (resolve-square-sit x exp)
    (if (check-nontrivial-sqrt1 x)
      0
      (expmod2 x exp m)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (resolve-square-sit (remainder (square base) m) (/ exp 2)))
        (else 
          (remainder (* base (expmod2 base (- exp 1) m))
                     m))))



;; if expo == 0 then (= expo 1) will return which is what we want
(define (miller-rabin-test n)
  (define (try-it-mr a)
    (= (expmod2 a (- n 1) n) 1))
  (try-it-mr (+ 1 (random (- n 1)))))


(define (miller-rabin-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else false)))

;;(miller-rabin-prime? 91 4)

;; 1.29

(define (inc a)
  (+ a 1))

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(define (simpsons-int f a b n)
  (define h (/ (- b a) n))
  (define (f-i i) 
    (f (+ a (* h i))))
  (define (simpson-term i)
    (cond ((or (= i 0) (= i n))
            (f-i i))
          ((even? i) (* 2 (f-i i)))
          (else (* 4 (f-i i)))))
  (* (/ h 3.0) (sum simpson-term 0 inc n)))


(define (simpsons-int f a b n)
  (define h (/ (- b a) n))
  (define (f-i i) 
    (f (+ a (* h i))))
  (define (simpson-term i)
    (* 3 (f-i i)))
  (* (/ h 3.0) (sum simpson-term 0 inc n)))

(simpsons-int cube 0 1 100)

(sum cube 1 inc 10)

;;1.31 a
(define (product-rec term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (pi-by-product prod-proc n)
  (define (pterm i)
    (if (even? i)
      (/ (+ 2 i) 
         (+ 1 i))
      (/ (+ 1 i)
         (+ 2 i))))
  (* 4.0
     (prod-proc pterm 1 inc n)))



;;1.31 b
(define (product-iter term a next b)
  (define (iter prod i)
    (if (> i b)
      prod
      (iter (* prod (term i))
            (next i))))
  (iter 1 1))


;;(pi-by-product product-rec 1000)
;;(pi-by-product product-iter 1000)

;;1.32 a
(define (accumulate1 combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate1 combiner null-value term (next a)  next b))))


;;1.32 b
(define (accumulate2 combiner null-value term a next b)
  (define (iter acc i)
    (if (> i b)
      acc
      (iter (combiner acc (term i)) (next i))))
  (iter null-value a))


(define (pi-by-acc acc-proc n)
  (define (pterm i)
    (if (even? i)
      (/ (+ 2 i) 
         (+ 1 i))
      (/ (+ 1 i)
         (+ 2 i))))
  (* 4.0
     (acc-proc * 1 pterm 1 inc n)))

;;(pi-by-acc accumulate1 1000)
;;(pi-by-acc accumulate2 1000)

;;1.33
(define (filter-accumulate combiner fpred null-value term a next b)
  (define (iter acc i)
    (if (> i b)
      acc
      (if (fpred i)
        (iter (combiner acc (term i)) (next i))
        (iter acc (next i)))))
  (iter null-value a))

;;1.33 a
(define (sum-prime-squares a b)
  (filter-accumulate + prime? 0 square a inc b))

;;(sum-prime-squares 5 10)

;;1.33 b
(define (sum-relative-primes n)
  (define (relative-prime-n x)
    (= 1 (gcd x n)))
  (filter-accumulate * relative-prime-n 1 identity 2 inc n))

;;(sum-relative-primes 10)

;;1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define phi 
  (fixed-point (lambda (x) 
                 (+ 1 (/ 1.0 x))) 1.0))

;;phi

;;1.36

(define x2x1000 
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define x2x1000ad (fixed-point (lambda (x) 
                    (average x (/ (log 1000) (log x)))) 2.0))


;;1.37 

(define (cont-frac n d k)
  (define (iter frac i)
    (cond ((= 0 i) frac)
          ((= k i) (iter (/ (n i) (d i)) (- i 1)))
          (else (iter 
                  (/ (n i) (+ frac (d i)))
                  (- i 1)))))
  (iter 0 k))


(define (cont-frac-iter n d k)
  (define (iter acc i)
    (cond ((= 0 i) acc)
          (else (iter (/ (n i) (+ (d i) acc))
                      (- i 1)))))
  (iter (/ (n k) (d k)) (- k 1)))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)


(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

;;1.38
(define (euler-e k)
  (+ 2
     (cont-frac-iter 
       (lambda (i) 1.0)
       (lambda (i)
         (if (= (remainder i 3) 2)
           (* (+ 1 (/ i 3))
              2)
           1))
       k)))

(euler-e 100)

;;1.39
(define (tan-cf x k)
  (cont-frac-iter 
    (lambda (i) 
      (if (= i 1)
        x
        (- (square x))))
    (lambda (i)
      (+ 1 
         (* 2 (- i 1))))
    k))

(tan-cf (/ pi 6) 10)

;;1.40

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) 
                 (* a (square x))
                 c)))

;;(newtons-method (cubic 2 3 4) 1.0)

;;((cubic 2 3 4) -2.5943130163548367)


;;1.41
(define (double f)
  (lambda (x) 
    (f (f x))))

((double inc) 1)

(((double (double double)) inc) 5)

;;1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

;;1.43
(define (repeated f n)
  (define (iter repeated-f  i)
    (if (= i n)
      repeated-f
      (iter (compose f repeated-f) (+ i 1))))
  (iter f 1))


(define (repeated2 f n)
  (if (= n 1)
    f
    (compose f 
             (repeated2 f (- n 1)))))


;;((repeated square 3) 5)
;;((repeated2 square 3) 5)

;;1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))


(define (smooth-n f n)
  (repeated (smooth f) n))

;;1.45
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (try x)
      (if (good-enough? x)
        x
        (try (improve x))))
    (try x)))

(define (fixed-point2 f first-guess)
  (define (close-enough? v1)
    (< (abs (- v1 (f v1))) tolerance))
  ((iterative-improve close-enough? f) first-guess))
  
(fixed-point2 (lambda (x) 
                (+ 1 (/ 1.0 x))) 1.0)

