(ns chain-reaction.core
  (:import [javax.swing JFrame JLabel]
           [java.awt Color GridLayout Color]
           [java.awt.event MouseListener]
           [javax.swing.border LineBorder]))

(defn neighbours
  ([[x y] [width height]]
    (reduce conj (for [i [1 -1]
                    :when (and (<= 0 (+ i y)) (< (+ i y) height))]
                       [x (+ i y)])
                  (for [i [1 -1]
                    :when (and (<= 0 (+ x i)) (< (+ x i) width))]
                       [(+ i x) y])))
  ([[x y] [width height] trash]
   (into '() (for [i (range -1 2) j (range -1 2)
                   :when (and (not (and (= j 0) (= i 0)))
                              (<= 0 (+ j y)) (> height (+ j y))
                              (<= 0 (+ i x)) (> width (+ i x)))]
                              [(+ x i) (+ j y)]))))

(defn add [[x y] mat]
  (let [v (nth (nth mat y) x)]
    (dosync (alter v assoc :population (inc (@v :population))))))

(defn label-update [n]
  (if (not (nil? (n :label)))
    (doto (n :label)
      (. setText (str (n :population)))
      (. setOpaque true)
      (. setBackground
         (Color. (min 255 (int (* (/ 255 (count (n :neighbours))) (n :population)))) 0 255))
      (. setBorder (LineBorder. Color/BLACK)))))


(defn set-up-cell [[x y] [width height] population diag]
   (add-watch (ref {:neighbours (if diag (neighbours [x y] [width height] :ig)
                                    (neighbours [x y] [width height]))
                      :population population})
   :explode
   (fn [k a o n]
     (if (>= (n :population) (count (n :neighbours)))
       (do
         (dosync (alter a assoc :population (- (n :population) (count (n :neighbours)))))
         (label-update {:neighbours (n :neighbours) :population 0 :label (n :label)})
         (dorun (for [i (n :neighbours)]
                  (add i (n :mat)))))
       (label-update n)))))

(defn make-0-mat [width height diag]
  (let [r (into [] (for [y (range height)]
             (into [] (for [x (range width)]
                        (set-up-cell [x y] [width height] 0 diag)))))]
    (dorun (for [x r v x] (dosync (alter v assoc :mat r))))
    r))

(defn const-adder [[x y]]
  (fn [mat]
    (add [x y] mat)))

(defn make-panel [[width height] diag]
  (let [mat (make-0-mat width height diag)
        frame (JFrame. "Chain Reaction!")]
    (. frame setLayout (GridLayout. width height))
    (. frame setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (. frame setSize 500 500)
    (dorun (for [x (range width) y (range height)]
             (dosync
               (alter (nth (nth mat y) x)
                     assoc :label (JLabel. (str (@(nth (nth mat y) x) :population))))
               (Thread/sleep 10)
               (. frame add (@(nth (nth mat y) x) :label)))))
    (. frame setVisible true)
    [frame mat]))

(defn run-panel [dims fns slp diag]
  (let [[frame mat] (make-panel dims diag)]
    (while true
      (do
        (dorun (map #(% mat) fns))
        (Thread/sleep slp)))))

(defn ui-panel [dims diag]
  (let [[frame mat] (make-panel dims diag)]
    (dorun (for [x (range (count mat)) y (range (count (nth mat x)))]
             (. (@(nth (nth mat y) x) :label)
                addMouseListener
                (proxy [MouseListener] []
                  (mouseClicked [e] nil)
                  (mouseEntered [e] nil)
                  (mouseExited [e] nil)
                  (mousePressed [e] nil)
                  (mouseReleased [e] (add [x y] mat))))))))

(run-panel [31 31] [(const-adder [15 15])] 100 true)
;(ui-panel [10 10] false)
