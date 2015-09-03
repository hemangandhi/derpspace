(ns op-sudoku-clj.core
  (:require clojure.set))
;op-sudoku-solver
;box: [val op [[x1 y1] [x2 y2]]]
;solution: [[x1 y1 val] [x2 y2 val]]
(defn row [n mat]
  "Get the nth row of a matrix."
  (nth mat n))

(defn col [n mat]
  "Get the nth column of a matrix."
  (map #(nth % n) mat))

(defn get-in-mat [[x y] mat]
  "Get the value at x, y in the matrix."
  (nth (row y mat) x))

(defn set-in-mat [[x y] mat val]
  "Set the value at x,y in the matrix. (No side-effects.)"
  (assoc mat y (assoc (row y mat) x val)))

(defn intersect? [[x1 y1 v1] [x2 y2 v2]]
  "See if any two values in a box are the same and of the same row or column."
  (and (= v1 v2) (or (= x1 x2) (= y1 y2))))

(defn row-col-test [prop]
  "Make sure that no two values in a box intersect as above."
  (let [len (count prop)]
    (not-any? #(= % true) (for [x (range len) y (range len) :when (not (= x y))]
             (intersect? (nth prop x) (nth prop y))))))

(defn op-test [prop op tar]
  "Make sure that the values in the box meet the requirements by the operator."
  (if (= op nil) (and (= 1 (count prop)) (= (nth (first prop) 2) tar))
    (let [val (apply op (map #(nth % 2) prop))]
         (if (= op /)
             (or (= val tar) (= val (/ 1 tar)))
             (if (= op -)
               (or (= val tar) (= val (* -1 tar)))
               (= val tar))))))

(defn full-box-test [prop box]
  "Test all parameters of a box."
  (and (row-col-test prop) (op-test prop (nth box 1) (first box))))

(defn incr-vec [vec ind]
  "Increment the value at ind in vec. (No side-effects.)"
  (assoc vec ind (+ 1 (nth vec ind))))

(defn all-perms [lsts]
  "Get all the combinations of values from a list of lists - one from each inner list."
  (if (not (not-any? empty? lsts))
    '()
    (loop [ret '()
           inds (vec (for [x lsts] 0))
           in 0]
      (if (<= (count (nth lsts in)) (nth inds in))
        (if (>= in (- (count inds) 1))
          ret
          (recur ret (incr-vec (assoc inds in 0) (+ 1 in)) (+ 1 in)))
        (recur (conj ret (for [x (range (count inds))] (nth (nth lsts x) (nth inds x))))
               (incr-vec inds 0) 0)))))

(defn get-possible [mat [x y]]
  "Get the possible values for a cell in a matrix without row/column repetition."
  (into '()
        (clojure.set/difference (set (range 1 (+ 1 (count mat))))
                                (set (row y mat))
                                (set (col x mat)))))

(defn solve-box [box mat]
  "Return all valid solutions for a given box in a matrix."
  (let [pts (nth box 2)
        perms (all-perms (map #(get-possible mat %) pts))]
    (filter #(full-box-test % box)
            (for [x perms]
              (for [y (range (count pts))]
                [(first (nth pts y)) (last (nth pts y)) (nth x y)])))))

;(solve-box [3 + [[0 1] [0 0]]] [[0 0] [0 0]])

(defn place-sol [sol mat]
  "Place a solution in the matrix. Does not check anything. No side-effects."
  (if (empty? sol) mat
    (place-sol (rest sol) (set-in-mat (butlast (first sol)) mat (last (first sol))))))

(defn mat-list [coll]
  (if (empty? coll) nil
      (if (instance? clojure.lang.PersistentVector (first coll))
        (conj (mat-list (rest coll)) (first coll))
        (conj (mat-list (first coll)) (mat-list (rest coll))))))

(defn min-ind-by-fn [fun coll]
  "Return the index in coll for which fun returns a minimum."
  (loop [ind 0 mi 0]
    (if (>= ind (count coll))
      mi
      (recur (inc ind) (if (< (fun (nth coll ind)) (fun (nth coll mi)))
                         ind mi)))))

(defn solve-mat
  "Solve a kenken puzzle of size dim and specified boxes."
  ([dim boxes] (solve-mat (into [] (for [x (range dim)] (into [] (for [y (range dim)] 0)))) boxes '()))
  ([mat boxes fail] (if (empty? boxes) mat
                      (let [sols (into [] (map #(solve-box % mat) boxes))
                            min-sol-ind (min-ind-by-fn count sols)
                            min-sol (nth sols min-sol-ind)
                            rem-box? #(= % (nth boxes min-sol-ind))]
                        (if (= 0 (count min-sol))
                          fail
                          (let
                            [tries (map #(solve-mat (place-sol % mat)
                                                    (remove rem-box? boxes)
                                                    fail) min-sol)
                             tries-list (remove nil? (mat-list tries))]
                            (remove #(or (and (coll? %) (empty? %)) (= fail %))
                                    tries-list)))))))

;(solve-mat 3 [[2 * [[0 0] [0 1] [1 1]]] [1 - [[1 0] [2 0]]] [9 + [[0 2] [1 2] [2 2] [2 1]]]])
