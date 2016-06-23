(def knight-moves [[2 1] [1 2]
                   [-2 1] [-1 2]
                   [2 -1] [1 -2]
                   [-2 -1] [-1 -2]])

(defn in-bounds? [[x y] [h w]]
  (and (<= 0 x) (< x h)
       (<= 0 y) (< y w)))

(defn moves-from-pt [pt dims]
  (filter #(in-bounds? % dims)
          (map #(map + % pt) knight-moves)))

(defn all-paths [[curr lst lst-n] dims]
  (map #(vec [(assoc curr % (inc lst-n)) % (inc lst-n)]) 
       (filter #(not (contains? curr %))
               (moves-from-pt lst dims))))

(defn knights-tours
  ([dims] (knights-tours dims (for [x (range (Math/ceil (first dims)))
                                    y (range (Math/ceil (second dims)))]
                                [{[x y] 0} [x y] 0])))
  ([dims state]
   (if (or (empty? state)
           (some #(= (apply * dims)
                     (nth % 2)) state))
     (map first state)
     (knights-tours dims
                    (mapcat #(all-paths % dims) state)))))
