(ns op-sudoku-clj.draw
  (:use op-sudoku-clj.core)
  (:import [javax.swing JFrame JLabel]
           [javax.swing.border LineBorder]
           [java.awt GridLayout Color]))

(defn do-frame [dim boxes]
  (let [sol (first (solve-mat dim boxes))
        frame (JFrame. "KenKen Solution")
        scl 50]
    (. frame setLayout (GridLayout. dim dim))
    (dorun (for [y (range dim) x (range dim)]
             (do (. frame add
                    (doto (JLabel. (str (get-in-mat [x y] sol)))
                      (. setBorder (LineBorder. Color/BLACK))
                      (. setOpaque true)
                      (. setBackground Color/WHITE))))))
    (. frame setVisible true)
    (. frame setSize (* dim scl) (* dim scl))
    frame))

(do-frame 3 [[2 * [[0 0] [0 1] [1 1]]] [1 - [[1 0] [2 0]]] [9 + [[0 2] [1 2] [2 2] [2 1]]]])
