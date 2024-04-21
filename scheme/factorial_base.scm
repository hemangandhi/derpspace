; Being a bit too interested in https://xkcd.com/2835/

(define (list-nth lst n)
  (if (nil? lst)
    #nil
    (if (=  n 0)
      (car lst)
      (list-nth (cdr lst) (- n 1)))))

(define (list-index-of lst n)
  (define (loop lst acc)
    (if (nil? lst)
      #nil
      (if (= n (car lst))
        acc
        (loop (cdr lst) (+ acc 1)))))
  (loop lst 0))

; Not needed -- but nice to have.
(define (unfactorial n)
  (define (loop acc n)
    (if (< n acc)
      (- acc 1)
      (loop (+ acc 1) (quotient n acc))))
  (loop 2 n))

(define (to-factorial-base digit-map n)
  (define (loop digit nn acc)
    (if (= 0 nn)
      acc
      (let ((next-digit (list-nth digit-map (remainder nn digit))))
        (if (nil? next-digit)
          'out-of-digits
          (loop (+ digit 1)
                (quotient nn digit)
                (cons next-digit acc))))))
  (loop 2 n #nil))

(define (from-factorial-base digit-map num)
  (define (loop num current-base total-base acc)
    (if (nil? num)
      acc
      (let ((idx-of-n (list-index-of digit-map (car num))))
        (if (nil? idx-of-n)
          'invalid-num
          (if (>= idx-of-n current-base)
            'num-too-large
            (loop (cdr num) (+ current-base 1) (* total-base current-base) (+ acc (* idx-of-n total-base))))))))
  (loop (reverse num) 2 1 0))
