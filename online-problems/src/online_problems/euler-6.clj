(ns online-problems.euler-6)

(defn euler-6 [x] (- (#(* % %) (apply + (range x))) 
  (apply + (map #(* % %) (range x)))))

