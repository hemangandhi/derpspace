(ns matrix-puzzle-solver.util.core
  (:gen-class))

(defmacro for-each-in-mat [mat body & {:keys [row-ind col-ind curr-val],
                                   :or {row-ind "r" col-ind "c" curr-val "v"}}]
  "Lets callers value a statement for each element in a matrix.
   r, c and v are temporary bindings to the row index, column index and current value.
   Those bindings may be altered through the keyword args."
  `(into [] (for [~(symbol row-ind) (range (count ~mat))]
              (into [] (for [~(symbol col-ind) (range (count (nth ~mat ~(symbol row-ind))))]
                         (let [~(symbol curr-val) (get-in ~mat [~(symbol row-ind) ~(symbol col-ind)])]
                           ~@body))))))

(defn in-bounds? [mat [x y]]
  (and (<= 0 x) (< x (count mat))
       (<= 0 y) (< y (count (nth mat x)))))

(defn get-col [mat col]
  "Get a column of a mat."
  (into [] (map #(nth % col) mat)))

(defn cols [mat]
  "Get all the columns of a mat."
  (for [x (range (count (first mat)))]
    (get-col mat x)))

(defn all-unique [coll]
  "Return whether everything is unique in a collection."
  (= (count coll) (count (into #{} (distinct coll)))))

(defn no-dups-in-rows [mat]
  "Check for duplication in all rows."
  (every? all-unique mat))

(defn no-dups-in-cols [mat]
  "Check for duplication in all cols."
  (every? all-unique (cols mat)))

(defn no-dup-rows [mat]
  "Make sure no rows are identical."
  (all-unique mat))

(defn no-dup-cols [mat]
  "Make sure no cols are identical."
  (all-unique (into [] (cols mat))))

(defn all-dir-vecs [include-diag?]
  (for [x (range -1 2) y (range -1 2)
        :when (if include-diag? (or (not= 0 x) (not= 0 y))
                (not= (Math/abs x) (Math/abs y)))] [x y]))

(defn neighbor-vecs [mat [x y] & {:keys [diag] :or {diag true}}]
  (for [x-a (range -1 2) y-a (range -1 2) :let [n-x (+ x x-a) n-y (+ y y-a)]
        :when (and (in-bounds? mat [n-x n-y])
                   (if diag (or (not= 0 x-a) (not= 0 y-a))
                     (not= (Math/abs x-a) (Math/abs y-a))))]
    [x-a y-a]))

(defn neighbors [mat [x y] & {:keys [diag] :or {diag true}}]
  (map #(map + [x y] %) (neighbor-vecs mat [x y] :diag diag)))

(defn neighbors-that-are-mat [mat fn diag?]
  (for-each-in-mat mat (list (count (filter #(fn v (get-in mat %)) (neighbors mat [r c] :diag diag?))))))

(defn even-count-in-row [row & objs]
  (apply = (map #(count (filter (fn [v] (= v %))
                                row)) objs)))

(defn even-count-in-all-rows [mat & objs]
  (every? #(apply even-count-in-row % objs) mat))

(defn even-count-in-all-cols [mat & objs]
  (every? #(apply even-count-in-row % objs) (cols mat)))

(defn counts-by-vec [mat pred vect]
  (for-each-in-mat mat (list (loop [ct 0 curr (into [] (map + [r c] vect))]
                           (if (or (not (in-bounds? mat curr)) 
                                   (not (pred v (get-in mat curr))))
                             ct
                             (recur (inc ct) (map + curr vect)))))))

(defn counts-by-vecs [mat red-fn pred vecs]
  (let [red-mats (map #(counts-by-vec mat pred %) vecs)]
    (into [] (for [x (range (count mat))]
               (into [] (for [y (range (count (nth mat x)))]
                          (reduce red-fn (map #(get-in % [x y]) red-mats))))))))
