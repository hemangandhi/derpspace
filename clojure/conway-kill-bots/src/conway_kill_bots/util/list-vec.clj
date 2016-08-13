(ns conway-kill-bots.util.list-vec)
"
(defn permutations
  ([coll] (permutations coll (count coll)))
  ([coll len] (if (= len 1) (for [x coll] [x])
                (for [x coll
                      y (permutations (remove #{x} coll) (dec len))]
                  (concat [x] y)))))

(defn slow-get-translation [shape1 shape2]
  (if (not= (count shape1) (count shape2))
    '()
    (ffirst
      (filter #(= 1 (count (distinct %))) 
              (map #(map (fn [x y] (map - y x)) shape1 %) 
                   (permutations shape2))))))
;slow is O(s*s!) where s is the shape length if they match...
;fast is O(s*log(s)).
"

(defn get-translation [shape1 shape2]
  (if (not= (count shape1) (count shape2)) '()
    (let [diff (map (fn [x y] (map - x y)) 
                    (sort shape2) (sort shape1))]
      (if (= 1 (count (distinct diff)))
        (first diff)
        '()))))

;rot-mat [[cos -sin] [sin cos]]

(def rotations {90 (fn [[x y]] [(* -1 x) y])
                180 (fn [[x y]] [(* -1 x) (* -1 y)])
                270 (fn [[x y]] [x (* -1 y)])})

