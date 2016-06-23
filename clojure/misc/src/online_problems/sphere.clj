;algorithm thanks to Divesh!
(defn distance [P1 P2]
  (Math/sqrt (apply + (map (fn [x1 x2] (Math/pow (- x1 x2) 2))
                           P1 P2))))

(defn pad-zero [seq]
  (lazy-seq (cons (if (empty? seq) 0 (first seq))
                  (pad-zero (rest seq)))))

(defn sq-pt-dist [Pt Sq-pt1 Sq-pt2]
  (let [dims (apply max (map count [Pt Sq-pt1 Sq-pt2]))
        [P Sq1 Sq2] (map #(if (= (count %) dims) %
                            (pad-zero %)) 
                         [Pt Sq-pt1 Sq-pt2])
        in-range (map (fn [x1 x2 p] (or (= x1 p) (= x2 p)
                                        (not= (neg? (- x1 p))
                                              (neg? (- x2 p)))))
                      Sq1 Sq2 P)]
    (distance (map (fn [in x1 x2 p]
                     (if in 0
                       (min (Math/abs (- x1 p))
                            (Math/abs (- x2 p)))))
                    in-range Sq1 Sq2 P)
              (for [x P] 0))))

(defn sq-sph-intersect [P rad Sq1 Sq2]
  (<= (sq-pt-dist P Sq1 Sq2) rad))
