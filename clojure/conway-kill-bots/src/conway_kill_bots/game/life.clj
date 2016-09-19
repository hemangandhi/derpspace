(ns conway-kill-bots.game.life
  (:require clojure.set))

(defn all-neighbours [[h w] [r c]]
  (for [x (range -1 2) y (range -1 2) :when (not (and (= x 0) (= y 0)))]
    [(mod (+ r x) h) (mod (+ c y) w)]))

(defn live-neighbours [mat [h w] [r c]]
  (count (filter #(.contains mat %) (all-neighbours [h w] [r c]))))

(defn next-life [mat [h w] & {:keys [live-rule spawn-rule]
                              :or {live-rule [2 3] spawn-rule [3]}}]
  (clojure.set/union (into #{} (filter #(.contains spawn-rule (live-neighbours mat [h w] %)) 
                                       (apply concat (map #(all-neighbours [h w] %) mat))))
                     (into #{} (filter #(.contains live-rule (live-neighbours mat [h w] %)) mat))))

(defn iter-life [drawer bot [h w] init]
 (loop [stg init gens 0 tot-add 0 tot-kill 0]
   (drawer stg [gens tot-add tot-kill])
   (let [del (bot stg)
         del+stg (clojure.set/difference (clojure.set/union stg del)
                                         (clojure.set/intersection stg del))
         next-s (next-life del+stg [h w])
         add-ct (count (clojure.set/difference del stg))
         kill-ct (- (count del) add-ct)]
     (if (every? #(= 0 %) next-s)
       [gens tot-add tot-kill]
       (recur next-s (inc gens) (+ tot-add add-ct) (+ tot-kill kill-ct))))))
