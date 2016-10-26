(ns matrix-puzzle-solver.core
  (:require [matrix-puzzle-solver.solver.general :as gen]
            [matrix-puzzle-solver.util.core :as util])
  (:gen-class))

(defn is-valid-0h_h1-end [state]
  (and (util/even-count-in-all-rows state :red :blue)
       (util/even-count-in-all-cols state :red :blue)
       (util/no-dup-rows state)
       (util/no-dup-cols state)
       (not-any? #(<= 2 %) (flatten (util/counts-by-vecs state max = (util/all-dir-vecs false))))))

(defn is-valid-0h_h1-mid [state]
  (let [half-r (/ (count state) 2) half-c (/ (count (first state)) 2)]
    (and (every? #(<= % half-r) 
                 (map #(count (filter (fn [v] (= v :red)) %))
                                      state))
         (every? #(<= % half-r) 
                 (map #(count (filter (fn [v] (= v :blue)) %))
                                      state))
         (every? #(<= % half-c) 
                 (map #(count (filter (fn [v] (= v :red)) %))
                                      (util/cols state)))
         (every? #(<= % half-c) 
                 (map #(count (filter (fn [v] (= v :blue)) %))
                                      (util/cols state)))
         (util/all-unique (filter #(not (contains? nil %)) state))
         (util/all-unique (filter #(not (contains? nil %)) (util/cols state)))
         (not-any? #(<= 2 %) (flatten (util/counts-by-vecs state
                                                           max
                                                           (fn [cu ot]
                                                             (and (not= cu nil)
                                                                  (= cu ot)))
                                                           (util/all-dir-vecs false)))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (gen/solve is-valid-0h_h1-mid 
                      [:red :blue] 
                      [[:red nil] [nil nil]])))
