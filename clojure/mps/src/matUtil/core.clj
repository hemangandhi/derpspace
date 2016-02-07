(ns matUtil.core)

(defmacro for-each-in-mat [mat body & {:keys [row-ind col-ind curr-val],
                                   :or {row-ind "r" col-ind "c" curr-val "v"}}]
  "Lets callers value a statement for each element in a matrix.
   r, c and v are temporary bindings to the row index, column index and current value.
   Those bindings may be altered through the keyword args."
  `(into [] (for [~(symbol row-ind) (range (count ~mat))]
              (into [] (for [~(symbol col-ind) (range (count (nth ~mat ~(symbol row-ind))))]
                         (let [~(symbol curr-val) (get-in ~mat [~(symbol row-ind) ~(symbol col-ind)])]
                           ~@body))))))

(defn get-col [mat col]
  "Get a column of a mat."
  (into [] (map (nth % col) mat)))

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
  (every? all-unique (cols mat))

(defn no-dup-rows [mat]
  "Make sure no rows are identical."
  (all-unique mat))

(defn no-dup-cols [mat]
  "Make sure no cols are identical."
  (all-unique (into [] (cols mat))))
