(ns twenty48.draw
  (:use twenty48.core)
  (:import [javax.swing JFrame JLabel JOptionPane JPanel]
           [javax.swing.border LineBorder]
           [java.awt GridLayout BorderLayout Color]))

(defn mk-label [txt]
  (doto (JLabel. (str txt))
          (. setBorder (LineBorder. Color/BLACK))
          (. setOpaque true)
          (. setBackground Color/WHITE)))

(defn make-ui
  "Makes a simple display ui with dimensions [h w]
   and scaling scl. Returns a 'printer' as specified
   in core/run-game."
  [[h w] scl]
  (let [frame (JFrame. "2048")
        upper (JPanel.)
        score-lbl (JLabel. "Score: ")
        lbl-mat (vec (for [x (range h)]
                       (vec (for [y (range w)] (mk-label 0)))))]
    (. upper setLayout (GridLayout. h w))
    (dorun (for [y (range h) x (range w)]
             (. upper add
               (get-in-mat lbl-mat [x y]))))
    (. frame add upper)
    (. frame add score-lbl BorderLayout/SOUTH)
    (. frame setVisible true)
    (. frame setSize (* h scl) (* w scl))
    (fn [mat sc]
      (if (= mat :game-over)
        (. score-lbl setText (str "Game over, score: " sc))
        (do (dorun (for [x (range h) y (range w)
                         :let [lbl (get-in-mat lbl-mat [x y])
                               n (get-in-mat mat [x y])
                               r (int (if (= 0 n) 0
                                        (Math/min (* 255 (/ (Math/log n)
                                                            (* 11 (Math/log 2))))
                                                  255.0)))]]
                     (do (. lbl setText (str n))
                       (. lbl setBackground
                          (Color. r (- 255 r) (- 255 r))))))
          (. score-lbl setText (str "Score: " sc)))))))

(defn opt-prompt
  "Prompts user for wasd using JOptionPanes."
  [m]
  (condp = (JOptionPane/showInputDialog "Use wasd to move:")
    "w" :left
    "a" :up
    "s" :right
    "d" :down
    (opt-prompt m)))

;(run-game [4 4] opt-prompt (make-ui [4 4] 200))
