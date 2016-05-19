;chapter 2-1

(define (make-rat n d) ;as of excercise 2-1
  (let ((neg (< 0 (* n d)))
        (g (gcd n d)))
    (cons (if neg (- (/ n g)) 
            (/ n g))
          (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat r1 r2)
  (make-rat (+ (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(define (sub-rat r1 r2)
  (make-rat (- (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;excercise 2.4

(define (cons-2-4 x y)
  (lambda (m) (m x y)))
(define (car-2-4 z)
  (z (lambda (p q) p)))
(define (cdr-2-4 z)
  (z (lambda (p q) q)))
