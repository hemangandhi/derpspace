(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

;excercise 1.1.7 -> 1.7

(define (better-good-enough? old-guess new-guess)
  (< (abs (- old-guess new-guess)) (/ old-guess 10)))

(define (better-sqrt-iter old-guess new-guess x)
  (if (better-good-enough? old-guess new-guess)
    new-guess
    (better-sqrt-iter new-guess (improve new-guess x) x)))

(define (sqrt-1-7 x)
  (better-sqrt-iter 1 (improve 1 x) x))

;excercise 1.1.7 -> 1.8

(define (improve-cu guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cu-root-iter old-guess new-guess x)
  (if (better-good-enough? old-guess new-guess)
    new-guess
    (cu-root-iter new-guess (improve-cu new-guess x) x)))

(define (cube-root x)
  (cu-root-iter 1 (improve 1 x) x))

; 1.3.4

(define (fixed-point f init-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
          next
          (try next))))
    (try init-guess)))

(define (deriv f)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) 
         dx))))

(define (newton-trans f)
  (lambda (x)
    (- x (/ (f x) ((deriv f) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-trans g) guess))

; excercise 1.3.4 -> 1.40

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; 1.41

(define (inc x) (+ x 1))

(define (double proc)
  (lambda (x) (proc (proc x))))

; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43

(define (repeated f n)
  (define (iter k acc)
    (if (not (< k n)) acc
      (iter (inc k) (compose acc f))))
  (iter 1 (lambda (x) x)))

; 1.44

(define (smoothen f dx)
  (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx)))
                 3)))

(define (n-smoothed n f dx)
  (define (t-smooth g) (smoothen g dx))
  ((repeated t-smooth n) f))

; 1.46

(define (iter-improve good-guess? improvment)
  (define (ret-val f init)
    (if (good-guess? init)
      init
      (ret-val (improvement init))))
  ret-val)

(define (fixed-point-2 f init)
  (let ((tolerance 0.00001))
    (iter-improve 
      (lambda (x) (< (abs (- x (f x))) tolerance))
      f)))
