(ns twenty48.core)

(defn get-in-mat
  "Get a value in a vector of vectors."
  [mat [x y]]
  (nth (nth mat x) y))

(defn assoc-in-mat
  "Assoc a value in a vector of vectors."
  [mat [y x] value]
  (assoc mat y (assoc (nth mat y) x value)))

(defn in-mat?
  "Make sure the index is in bounds.
   Returns :x if x or x and y are out of bounds,
   and :y if only y is out of bounds."
  [mat [x y]]
  (if (and (< x (count (nth mat 0))) (>= x 0)
           (< y (count mat)) (>= y 0))
    true
    (if (and (< y (count mat)) (>= y 0))
      :x
      :y)))

(def dirs '(:up :down :left :right))

(defn shift-row
  "Shifts a row according to 2048 game play."
  [row]
  (loop [i 1 temp row lep 0 score 0]
    (if (>= i (count row))
      [temp score]
      (if (= 0 (nth temp i))
        (recur (inc i) temp lep score)
        (let [[nxt-t next-m ds] (loop [j i]
                               (if (< j lep)
                                [temp j]
                                (if (or (= j lep) (not (= 0 (nth temp (dec j)))))
                                  (if (and (= i j) (not (= (nth temp i) (nth temp (dec j)))))
                                    [temp i 0]
                                    (if (and (> j lep) (= (nth temp i) (nth temp (dec j))))
                                      [(assoc (assoc temp (dec j) (* 2 (nth temp i))) i 0) j (* 2 (nth temp i))]
                                      [(assoc (assoc temp j (nth temp i)) i 0) j 0]))
                                  (recur (dec j)))))]
          (recur (inc i) nxt-t next-m (+ score ds)))))))

(defn shift-up
  "Shift a column up within a matrix."
  [mat col-ind]
  (let [[col s] (shift-row (vec (for [x (range (count mat))]
                   (get-in-mat mat [x col-ind]))))]
    (loop [i 0 acc mat]
      (if (>= i (count col))
        [acc s]
        (recur (inc i) (assoc-in-mat acc [i col-ind] (nth col i)))))))

(defn move
  "Perform a 2048 move - :up, :down, :left or :right."
  [mat dir]
  (condp = dir
    :left (let [v (for [x mat] (shift-row x))]
            [(vec (map first v)) (apply + (map second v))])
    :right (let [rev (vec (map #(vec (reverse %)) mat))
                 v (move rev :left)]
             [(vec (map #(vec (reverse %)) (first v))) (last v)])
    :up (loop [i 0 acc mat sc 0]
          (if (>= i (count (nth mat 0)))
            [acc sc]
            (let [v (shift-up acc i)]
              (recur (inc i) (first v) (+ sc (last v))))))
    :down (let [mv (move (vec (reverse mat)) :up)]
            [(vec (reverse (first mv))) (last mv)])))

(defn rand-spawn
  "Randomly spawn from a selection, selec into mat.
   Altering selec allows for adjustment of the probabilites."
  [mat selec]
  (let [cells-0 (vec (for [x (range (count mat)) y (range (count (nth mat 0)))
                           :when (= 0 (get-in-mat mat [y x]))] [y x]))]
    (assoc-in-mat mat (rand-nth cells-0) (rand-nth selec))))

(defn run-game
  "Runs a game of given dimesions with mover,
   a function that takes in the matrix and
   returns a move; and, printer, a function
   that takes the matrix and score (or :game-over
   as the matrix)."
  [[h w] mover printer]
  (let [sp-arr [2 2 4]
        mat-0 (vec (for [x (range h)] (vec (for [y (range w)] 0))))]
    (loop [mat (rand-spawn (rand-spawn mat-0 sp-arr) sp-arr) sc 0]
      (printer mat sc)
      (if (and (every? #(every? (fn [v] (not (= 0 v))) %) mat)
               (every? #(= 0 (last %)) (map #(move mat %) dirs)))
        (printer :game-over sc)
        (let [mv (move mat (mover mat))]
          (if (= (first mv) mat)
            (recur mat sc)
            (recur (rand-spawn (first mv) sp-arr) (+ sc (last mv)))))))))
