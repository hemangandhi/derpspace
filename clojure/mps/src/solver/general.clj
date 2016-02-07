(ns solver.general
  :use matUtil.core
  (:gen-class))

(defn mat-to-poss [known poss] 
  "Replace nils with all possibilities."
  (for-each-in-mat known (list (if (nil? v) poss v))))

(defn poss-to-mat [mat]
  "Replace those with only 1 possibility to that possibility."
  (for-each-in-mat mat (list (if (and (coll? v) (= 1 (count v))) (first v) v))))

(defn filter-mat-poss [mat validator]
  "Filter out invalid possibilities."
  (for-each-in-mat mat (list (if (coll? v) 
                               (filter #(validator (assoc-in mat [r c] (first %))) v) 
                               v))))

(defn least-poss-in-mat [mat exclude-max]
  "Gets a vec of [row col] of the element with the least possibilities (excluding 0's using exclude-max)."
  (first (keys (apply min-key #(first (vals %)) 
                 (flatten 
                   (for-each-in-mat mat (list {[r c] (if (coll? v) 
                                                       (count v)
                                                       exclude-max)})))))

(defn poss-to-mat-vec [mat exclude-max]
  "Takes a matrix of possibilities and returns every possibility."
  (let [p (poss-to-mat mat)]
    (if (not-any? identity (for-each-in-mat p (list (coll? v))))
      [p]
      (let [l (least-poss-in-mat p exclude-max)]
        (conj (poss-to-mat-vec (assoc-in p l (rest (get-in p l)))) 
              (assoc-in p l (first (get-in p l))))))))

(defn solve [validator states known]
  "Return all possible solutions of the matrix puzzle, given the validator, states, and matrix
   of known values."
  (filter validator (poss-to-mat-vec (if (some? nil? (flatten known))
                                       (mat-to-poss known) 
                                       known)
                                     (inc (count states)))))))
