;; exercise 1.1

10 ;; => 10

(+ 5 3 4) ;; => 12

(- 9 1) ;; => 8

(/ 6 2) ;; => 3

(+ (* 2 4) (- 4 6)) ;; => 6

(define a 3)

(define b (+ a 1))

(+ a b (* a b)) ;; => 19

(= a b) ;; #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;; => 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; => 16

(+ 2 (if (> b a) b a)) ;; => 6

(* (cond ((> a b) a)
          ((< a b) b)
          (else -1))
   (+ a 1))
;; => 16


;; exercise 1.2

(/ (+ 5 4 (- 2
             (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; -37/150

;; exercise 1.3

(define two-larger-numbers-of-sum
  (lambda (sort-ls)
    (let ((ls (sort sort-ls >)))
      (+ (car ls) (car (cdr ls))))))

(define sort
  (lambda (ls eq)
    (sort-iter ls eq (length ls))))

(define sort-iter
  (lambda (ls eq count)
    (if (= count 0)
        ls
        (sort-iter (bubble-sort ls eq) eq (- count 1)))))

(define bubble-sort
  (lambda (ls eq)
    (if (null? (cdr ls))
        ls
        (let ((sort-ls (eq-larger-number (car ls) (car (cdr ls)) eq))
              (new-ls (cdr (cdr ls))))
          (cons (car sort-ls) (bubble-sort (cons (cdr sort-ls) new-ls) eq))))))


(define eq-larger-number
  (lambda (one two eq)
    (if (eq one two)
        (cons one two)
        (cons two one))))

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+
         (length (cdr ls))
         1))))


(two-larger-numbers-of-sum (list 1 3 2)) ;; => 5


(sort (list 1 2 3 2 3 482 111 333) <) ;; => (1 2 2 3 3 111 333 482)

(sort (list 33 111 9403 193 18333 28394) >) ;; => (28394 18333 9403 193 111 33)


;; 1.1.7

(define sqrt
  (lambda (x)
    (sqrt-iter x 1.0)))

(define sqrt-iter
  (lambda (x guess)
    (if (good-enough? guess x)
        guess
        (sqrt-iter x (improve guess x)))))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (squaer guess) x)) 0.001)))

(define squaer
  (lambda (x)
    (* x x)))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(sqrt 9) ;; => 3.00009155413138
