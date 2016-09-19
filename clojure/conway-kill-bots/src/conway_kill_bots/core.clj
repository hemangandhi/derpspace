(ns conway-kill-bots.core
  (:use conway-kill-bots.game.life)
  (:import [javax.swing JFrame JLabel JPanel]
           [javax.swing.border LineBorder]
           [java.awt GridLayout BorderLayout Color])
  (:gen-class))

(defn mk-label []
  (doto (JLabel.)
    (. setOpaque true)
    (. setBackground Color/WHITE)
    (. setBorder (LineBorder. Color/BLACK))))

(defn simple-draw [[h w] & {:keys [delay scale]
                            :or {delay 1000 scale 50}}]
  (let [frame (JFrame. "Conway killer!")
        upper (JPanel.)
        score-lbl (JLabel. "Current score: ")
        lbl-mat (vec (for [x (range h)] (vec (for [y (range w)] (mk-label)))))]
    (. upper setLayout (GridLayout. h w))
    (dorun (map (fn [row] (dorun (map #(. upper add %) row))) lbl-mat))
    (. frame add upper)
    (. frame add score-lbl BorderLayout/SOUTH)
    (. frame setVisible true)
    (. frame setSize (* h scale) (* w scale))
    (fn [stage [gen kills spws]]
      (dorun (for [x (range h) y (range w)]
               (. (get-in lbl-mat [x y]) setBackground
                 (if (contains? stage [x y]) Color/RED Color/WHITE))))
      (. score-lbl setText (str "Current score: Generations: " gen
                                " Direct kills: " kills
                                " Spawned: " spws))
      (Thread/sleep delay))))

(defn -main [& args]
  (iter-life (simple-draw [10 10] :delay 10) (fn [z] #{}) [10 10] #{[0 1] [1 2] [2 0] [2 1] [2 2]}))
