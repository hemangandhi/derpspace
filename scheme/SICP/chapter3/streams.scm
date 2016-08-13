(define the-empty-stream (delay '()))

(define (stream-cons a b) (cons a (delay b)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-null? s) (eq? the-empty-stream s))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . args)
  (if (stream-null? (car args))
    the-empty-stream
    (stream-cons (apply proc (map stream-car args))
                 (stream-map proc (map stream-cdr args)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s)) (stream-cons (stream-car s)
                                            (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (scale-stream stream scale)
  (stream-map (lambda (x) (* x scale)) stream))

(define (mul-streams . strs) (stream-map * strs))
(define (add-streams . strs) (stream-map + strs))
(define ones (stream-cons 1 ones)) ;is somehow unbound, though quoted from the book...
(define integers (stream-cons 1 (add-streams integers ones)))

;exercise 3.54
(define factorials (stream-cons 1 (mul-streams factorials (stream-cdr integers))))

;exercise 3.55
(define (partial-sums stream)
  (define ret (stream-cons 0 (add-streams stream ret)))
  (stream-cdr ret))

;execise 3.64
(define (stream-limit stream toler)
  (let ((s1 (stream-car stream)) (s2 (stream-car (stream-cdr stream))))
    (if (> toler (abs (- s1 s2)))
      s2
      (stream-limit (stream-cdr stream) toler))))

;quoted from page 289
(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (stream-cons (stream-car s1)
                 (stream-append (stream-cdr s1) s2))))

(define (stream-interleave s1 s2)
  (if (stream-null? s1) s2
    (cons-stream (stream-car s1)
                 (stream-interleave s2 (stream-cdr s1)))))

(define (stream-pairs s t)
  (stream-cons
    (list (stream-car s) (stream-car t))
    (stream-interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (stream-pairs (stream-cdr s) (stream-cdr t)))))

;exercise 3.76
(define (smoothen stream)
  (scale-stream (add-streams stream (stream-cdr stream))
                0.5))
