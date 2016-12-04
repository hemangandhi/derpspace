
(define (consify ls)
  (cond ((not (list? ls)) ls)
        ((null? ls) '())
        (else (list 'cons (consify (car ls)) (consify (cdr ls))))))

(consify '(a g (c d (e f (b)))))
