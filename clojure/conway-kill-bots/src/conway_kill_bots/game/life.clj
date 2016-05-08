(ns game.life
  (:require clojure.set))

(defn all-neighbours [[h w] [r c]]
  (for [x (range -1 2) y (range -1 2) :when (not (and (= x 0) (= y 0)))]
    [(mod (+ r x) h) (mod (+ c y))]))

(defn live-neighbours [mat [h w] [r c]]
  (count (filter #(.contains mat %) (all-neighbours h w [r c]))))

(defn next-life [mat [h w]]
  (clojure.set/union (into #{} (filter #(= 3 (live-neighbours mat [h w] %)) 
                                       (flatten (map (all-neighbours [h w] %) mat))))
                     (into #{} (filter #(or (= 2 (live-neighbours mat [h w] %)) (= 3 (live-neighbours mat [h w] %))) mat))))
