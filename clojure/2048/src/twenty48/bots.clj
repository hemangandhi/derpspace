(ns twenty48.bots
  (:use [twenty48.core]
        [twenty48.draw]))

(defn wrap-bot
  "Wraps the bot fn to induce a
   delay of turn-delay ms."
  [bot-fn turn-delay]
  (fn [m]
    (Thread/sleep turn-delay)
    (bot-fn m)))

(defn move-map
  "Maps each move direction to the value returned by move."
  [m]
  (into {} (for [x dirs] [x (move m x)])))

(defn filter-moves-by-mat
  "Filter the resulting moves by their matrices according to pred."
  ([m pred]
    (filter-moves-by-mat m pred (move-map m)))
  ([m pred lst]
    (into {} (filter #(pred (first (second %))) lst))))

(defn filter-null
  "Filter out moves that don't change the board."
  [m]
  (filter-moves-by-mat m #(not (= m %))))

(defn rand-mover
  "Picks a random move that alters the board every turn."
  [m]
  (rand-nth (keys (filter-null m))))

(defn highest-score-bot
  "Pick the highest scoring move (prefers down and right).
   Pass in a move map to constrain the bot."
  ([m]
    (highest-score-bot m (filter-null m)))
  ([m l]
    (first (apply max-key #(second (val %)) l))))

(defn keep-cell
  "All the moves that preserve cell
   while not being null."
  [m [x y]]
  (filter-moves-by-mat m #(<= (get-in-mat m [x y])
                           (get-in-mat % [x y]))
                       (filter-null m)))

(defn high-corner-bot
  "Plays into bottom right if possible,
   else plays by highest-score-bot."
  [[x y]]
  (fn [m]
    (let [con (keep-cell m [x y])]
      (if (or (empty? con)
              (not (= (apply max (map #(apply max %) m))
                      (get-in-mat m [x y]))))
        (highest-score-bot m)
        (highest-score-bot m con)))))

(run-game [4 4] (wrap-bot (high-corner-bot [3 3]) 100) (make-ui [4 4] 100))
