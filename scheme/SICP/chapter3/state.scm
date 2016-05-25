;exercise 3.1
(define (make-accumulator init)
  (lambda (amount)
    (set! init (+ init amount))
    init))

;3.2
(define (make-monitored f)
  (let ((ct 0))
    (lambda (val)
      (if (symbol? val)
        (cond ((eq? val 'how-many-calls?) ct)
              ((eq? val 'reset-count) (set! ct 0))
              (else (begin
                      (set! ct (+ 1 ct))
                      (f val))))
        (begin (set! ct (+ 1 ct))
               (f val))))))

(define (make-seer)
  (define acc '())
  (define (seen? val)
    (define (iter val lst)
      (cond ((null? lst) (append! acc val) #f)
            ((eq? (car lst) val) #t)
            (else (iter val (cdr lst)))))
    (iter val acc)))

;3.17
(define (count-pairs struct)
  (define seen? (make-seer))
  (define (iter struct)
    (if (or (not (pair? struct)) (seen? struct))
      0
      (+ (iter (car struct))
         (iter (cdr struct))
         1)))
  (iter struct))

;3.18
(define (has-cycle? struct)
  (define seen? (make-seer))
  (define (iter struct)
    (cond ((null? struct) #f)
          ((seen? struct) #t)
          (else (iter (cdr struct)))))
  (iter struct))

;3.19 (Thanks, Floyd..)
(define (th-has-cycle? struct)
  (define (safe-cdr v)
    (if (pair? v)
      (cdr v)
      '()))
  (define (iter a b)
    (cond ((not (pair? a)) #f)
          ((not (pair? b)) #f)
          ((eq? a b) #t)
          ((eq? a (safe-cdr b)) #t)
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
  (iter struct (safe-cdr struct)))


