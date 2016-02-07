(ns matrix-puzzle-solver.core
  (:require [matrix-puzzle-solver.solver.general :as gen]
            [matrix-puzzle-solver.util.core :as util])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (gen/poss-to-mat-vec [[2 [1 3 4] 6] [10 [11 12 14] 16]] 
                                #(every? identity (flatten (util/for-each-in-mat % 
                                                           (list (= (mod 2 v) 0))))) 
                                4)))
