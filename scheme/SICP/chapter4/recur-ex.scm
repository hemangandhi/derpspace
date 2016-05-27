;exercise 4.21

;a -> fibonacci
(display
  ((lambda (n)
    ((lambda (fib)
       (fib fib 1 1 n))
     (lambda (f a b n)
       (cond ((= n 0) a)
             ((= n 1) b)
             (else (f f b (+ a b) (- n 1)))))))
   10)) ;89, as desired...

;b -> even and odd...
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))
