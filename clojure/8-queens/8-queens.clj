(ns eight-queens
  (:import [javax.swing JPanel JFrame]
           [java.awt Graphics Color]))

(defn queens-intersect? [[x1 y1] [x2 y2]]
  (boolean (or (= x1 x2)
               (= y1 y2)
               (= (Math/abs (- x1 x2))
                  (Math/abs (- y1 y2))))))

(defn solve-queens
  ([dim] (solve-queens (into [] (for [x (range dim)] [x 0])) 0))
  ([queens curr-ind]
   (if (= (count queens) curr-ind)
     queens
     (loop [i (last (nth queens curr-ind))]
       (if (= i (count queens))
         nil
         (if (not-any? true? (map #(queens-intersect? % [curr-ind i]) (take curr-ind queens)))
           (let [res (solve-queens (assoc queens curr-ind (assoc (nth queens curr-ind) 1 i)) (inc curr-ind))]
             (if (not (nil? res))
               res
               (recur (inc i))))
           (recur (inc i))))))))



(defn make-panel [dim scl]
  (proxy [JPanel] []
    (paintComponent [#^Graphics g]
        (dorun (for [x (range dim) y (range dim)]
                 (do
                   (if (= (mod x 2) (mod y 2))
                       (. g setColor Color/WHITE)
                       (. g setColor Color/BLACK))
                   (. g fillRect (* x scl) (* y scl) scl scl))))
        (. g setColor Color/RED)
        (dorun (map #(. g fillOval (* (first %) scl) (* (last %) scl) scl scl)
                    (solve-queens dim))))))

(defn run-queens [dim]
  (let [scl 75]
    (doto (JFrame. "Queens!")
      (. setSize (* dim scl) (* dim scl))
      (.add (make-panel dim scl))
      (. setVisible true)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE))))

(run-queens 5)
