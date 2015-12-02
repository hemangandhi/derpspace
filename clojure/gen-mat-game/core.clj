(ns gen-mat-game.core)

(defn play-mat-game [[h w] init out in]
  (let [[mat st] (init [h w])]
    (loop [t mat s st]
      (out mat st)
      (let [[n-t n-s] (in mat st)]
        (if (= n-s :game-over)
          (out mat :game-over)
          (recur n-t n-s))))))
