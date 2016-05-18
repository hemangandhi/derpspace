(define (dec x) (- x 1))

;excercise 1-2-2 -> 1-11.

(define (f-1-11-rec n)
  (if (< n 3) n
    (+ (f (dec n)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f-1-11-iter n)
  (define (tf cwn n-m-1 n-m-2 n-m-3)
    (if (< cwn 0)
      n-m-1
      (tf (dec cwn) (+ n-m-1 (* 2 n-m-2) (* 3 n-m-3))
          n-m-1 n-m-2)))
  (if (< n 3) n
    (tf (- n 3) 3 2 1)))

;excercise 1-2-4 -> 1-16

(define (fast-exp b n)
  (define (hlp ib in acc)
    (cond ((= in 0) acc)
          ((even? in) (hlp (* ib ib) (/ in 2) acc))
          (else (hlp ib (dec in) (* acc ib)))))
  (hlp b n 1))

; 1-17

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-mult-rec a b)
  (cond ((= 0 a) 0)
        ((even? a) (double (fast-mult-rec (halve a) b)))
        (else (+ a (fast-mult-rec (dec a) b)))))

; 1-18

(define (fast-mul a b)
  (define (hlp ia ib acc)
    (cond ((= 0 ia) acc)
          ((even? ia) (hlp (halve ia) (double ib) acc))
          (else (hlp (dec ia) ib (+ ib acc)))))
  (hlp a b 0))

; 1-3-1 1-30

(define (sum term start next end)
  (define (iter a acc)
    (if (> a end) acc
      (iter (next a) (+ (term a) acc))))
  (iter start 0))

; 1-3-1 1-29

(define (cube x) (* x x x))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (term x) 
    (define fv (f (+ a (* x h))))
    (cond ((= x 0) fv)
          ((= x n) fv)
          ((even? x) (* 2 fv))
          (else (* 4 fv))))
  (define (next x) (+ 1 x))
  (* (/ h 3) (sum term 0 next n)))

; 1-32

(define (accumulate combiner null-val term start next end)
  (define (iter ct acc)
    (if (> ct end) acc
      (iter (next ct) (combiner acc (term ct)))))
  (iter start null-val))

; 1-33

(define (filter-accumulate pred combiner null-val term start next end)
  (define (iter ct acc)
    (cond ((> ct end) acc)
          ((pred ct) (iter (next ct) (combiner acc (term ct))))
          (else (iter (next ct) acc))))
  (iter start null-val))
