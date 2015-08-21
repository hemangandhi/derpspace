(ns op-sudoku-clj.gen-puzzle
  (:use op-sudoku-clj.core))

(defn pick-rand
  ([coll] (nth coll (Math/floor (* (Math/random) (count coll)))))
  ([a b] (+ (min a b) (Math/floor (* (Math/random) (Math/abs (- a b)))))))


(defn random-mat [dim]
  (loop [i 0
         mat (into [] (for [x (range dim)]
                        (into [] (for [y (range dim)] 0))))]
    (if (>= i dim)
      mat
      (recur (inc i)
             (loop [inner mat indices (into '() (range dim))]
               (if (empty? indices)
                 inner
                 (let [posses (into '() (for [ind indices] [ind (get-possible inner [ind i])]))
                       min-col (min-ind-by-fn #(count (last %)) posses)]
                 (recur (set-in-mat [min-col i] inner (pick-rand (last (nth posses min-col))))
                        (into '() (remove #(= (first (nth posses min-col)) %) indices))))))))))

(random-mat 9)
