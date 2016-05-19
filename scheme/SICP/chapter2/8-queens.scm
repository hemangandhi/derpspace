(define (accumulate op init seq) ;this is reduce...
  (if (null? seq) init
    (op (car seq)
        (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval from to)
  (define (iter cur acc)
    (if (< cur from) acc
      (iter (- cur 1) (cons cur acc))))
  (iter to (list)))

;excercise 2.42

(define empty-board (list))

(define (qu-col queen)
  (car (cdr queen)))

(define (checking? q1 q2)
  (or (= (car q1) (car q2))
      (= (qu-col q1) (qu-col q2))
      (= (abs (- (car q1) (car q2)))
         (abs (- (qu-col q1) (qu-col q2))))))

(define (get-queen-by-col col queens)
  (car (filter (lambda (x) (= (qu-col x) col))
          queens)))

(define (same? q1 q2)
  (and (= (car q1) (car q2))
       (= (qu-col q1) (qu-col q2))))

(define (safe? col positions)
  (let ((qu (get-queen-by-col col positions)))
    (newline)
    (display positions)
    (newline)
    (display col)
    (null? (filter (lambda (x) (and (checking? x qu)
                                    (not (same? x qu))))
                   positions))))

(define (adjoin-position new-row col rest)
  (cons (list new-row col) rest))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter (lambda (positions)
                (safe? k positions))
              (flatmap (lambda (rest-of-queens)
                         (map (lambda (new-row)
                                (adjoin-position new-row k rest-of-queens))
                              (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))
