(ns better-q)

(defn generate
  ([l] (generate l l))
  ([len left]
    (if (= 0 left) [[]]
        (for [x (range len) y (generate len (dec left))]
          (conj y x)))))

(defn not-checking [q qs]
  (and (not (.contains qs q))
       (not-any? (fn [[idx v]] (= (Math/abs (- q v))
                                  (inc idx)))
                 (map-indexed vector qs))))

(defn test-q [qs]
  (if (empty? qs) true
      (and (not-checking (first qs) (rest qs))
           (test-q (rest qs)))))

(defn n-queens [n]
  (filter test-q (generate n)))
