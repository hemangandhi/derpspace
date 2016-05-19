;2.2 -> 2.17

(define (last-pair ls)
  (if (null? (cdr ls))
    ls
    (last-pair (cdr ls))))

; 2.18

(define (reverse-2-18 ls)
  (if (null? ls) ls
    (cons (reverse (cdr ls)) (car ls))))

; 2.23

(define (for-each-2-23 proc ls)
  (cond ((not (null? ls)) (proc (car ls))
                          (for-each-2-23 proc (cdr ls)))))

; 2.27

(define (deep-reverse ls)
  (cond ((null? ls) ls)
        ((not (pair? (car ls))) 
         (cons (deep-reverse (cdr ls)) (car ls)))
        (else (cons (deep-reverse (cdr ls)) (deep-reverse (car ls))))))

;2.28

(define (fringe tree)
  (define (iter tr acc)
    (cond ((null? tr) acc)
          ((not (pair? (car tr))) 
           (iter (cdr tr) (append acc (car tr))))
          (else (iter (cdr tr) (append acc (fringe (car tr)))))))
  (iter tree (list)))

;2.29 -> Binary mobile

(define (make-branch length structure)
  (list length structure))

(define (structure branch)
  (cdr branch))
(define (branch-len branch)
  (car branch))

(define (make-mobile left right)
  (list left right))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (total-weight mobile)
  (define (branch-w branch)
    (cond ((null? branch) 0)
          ((not (pair? (structure branch))) (structure branch))
          (else (total-weight (structure branch)))))
  (if (null? mobile) 0
    (+ (branch-w (left-branch mobile))
       (branch-w (right-branch mobile)))))

(define (balanced? mobile)
  (or (null? mobile)
    (= (* (branch-len (left-branch mobile))
          (total-weight (structure (left-branch mobile))))
       (* (branch-len (right-branch mobile))
          (total-weight (structure (right-branch mobile)))))))

;2.30

(define (square-tree ls)
  (map (lambda (x) (if (not (pair? x)) (* x x)
                     (square-tree x))) ls))

;2.31

(define (tree-map proc tree)
  (map (lambda (x) (if (not (pair? x)) (proc x)
                     (tree-map proc x))) tree))

;2.32

(define (subsets s)
  (if (null? s) (list)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (accumulate op init seq) ;this is reduce...
  (if (null? seq) init
    (op (car seq)
        (accumulate op init (cdr seq)))))

;2.33

(define (map-2-33 p seq)
  (accumulate (lambda (x y) (cons (p x) y)) (list) seq))

(define (append-2-33 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-2-33 seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;2.34

(define (horner-eval x coeffs)
  (accumulate (lambda (cur high) (+ high (* x cur)))
              0
              coeffs))

;2.35

(define (count-leaves tree)
  (accumulate + 0
              (map (lambda (x) (if (pair? x) (count-leaves x) 1)) tree)))

;2.36

(define (accumulate-n op init seq)
  (if (null? (car seq)) (list)
    (cons (accumulate op init (map car seq))
          (accumulate-n op init (map cdr seq)))))

;2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector n x)) m)))

;utils for 2.40 +

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (remove v s) (filter (lambda (x) (not (= v s)))
                             s))

(define (permutations s)
  (if (null? s) (list)
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

(define (enumerate-ints from to)
  (define (iter cur acc)
    (if (= cur from) acc
      (iter (- cur 1) (cons cur acc))))
  (iter to (list)))

;2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-ints 1 i)))
           (enumerate-ints 1 n)))

;2.41

(define (unique-n-tuples n max)
  (define (extend-tup tups)
    (flatmap (lambda (x)
               (map (lambda (i) (cons i x))
                    (enumerate-ints 1 (car x))))
             tups))
  (cond ((= n 0) (list))
        ((= n 1) (enumerate-ints 1 max))
        (else (extend-tup (unique-n-tuples (- n 1) max)))))

(define (sum-thing n s)
  (filter (lambda (x) (= s (accumulate + 0 x)))
          (unique-n-tuples 3 n)))
