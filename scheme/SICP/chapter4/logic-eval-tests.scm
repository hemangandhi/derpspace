;inspired by the book, not of it.

(define (frame-contains? frame var)
  (if (null? frame) #f
    (if (eq? (caar frame) var)
      (cadar frame)
      (frame-contains? (cdr frame var)))))

(define (add-to-frame frame var val)
  (if (not (frame-contains? frame var))
    (cons (cons var val) frame)
    'dup-key))

(define (var? sym) (string-prefix? "?" (symbol->string sym)))

(define (pattern-match pattern match frame)
  (cond ((symbol? frame) frame)
        ((and (null? pattern) (null? match)) frame)
        ((or (null? pattern) (null? match)) 'no-match)
        ((equal? (car pattern) (car match))
         (pattern-match (cdr pattern) (cdr match) frame))
        ((var? (car pattern))
         (let ((var (frame-contains? frame (car pattern))))
           (if var
             (if (equal? var (car match))
               (pattern-match (cdr pattern) (cdr match) frame)
               'no-match)
             (pattern-match (cdr pattern) (cdr match)
                            (add-to-frame frame (car pattern) (car match))))))
        ((and (list? (car pattern))
              (list? (car match)))
         (pattern-match (cdr pattern) (cdr match)
                        (pattern-match (car pattern) (cdr match)
                                       frame)))
        (else 'no-match)))
