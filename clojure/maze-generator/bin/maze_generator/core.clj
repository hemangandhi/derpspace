(ns maze-generator.core
  (:import [javax.swing JPanel JFrame]
           [java.awt Graphics Color]))


(defn get-neighbour
  "Gets neighbours of node [row col] within grid of dims [max-x max-y] and excludes members of bans."
  [[row col] [max-x max-y] bans]
   (reduce conj (for [x [1 -1]
                  :when (and (<= 0 (+ x row)) (< (+ x row) max-x)
                             (not (.contains bans [(+ row x) col])))]
                     [(+ row x) col])
                (for [x [1 -1]
                  :when (and (<= 0 (+ x col)) (< (+ x col) max-y)
                             (not (.contains bans [row (+ x col)])))]
                     [row (+ col x)])))

(defn get-random
  "Gets random element of a collection or nil for empty collections."
  [coll]
  (if (or (not (coll? coll)) (= 0 (count coll)))
    nil
    (nth coll (int (* (count coll) (java.lang.Math/random))))))

(defn take-first
  "Get the first element of coll that meets pred"
  [pred coll] (if (or (not (coll? coll)) (empty? coll))
                nil
                (if (pred (first coll))
                  (first coll)
                  (take-first pred (rest coll)))))

(defn make-maze
  "Makes maze of width and height."
  ([width height] (make-maze width height [0 0] '() '()))
  ([width height pos mem conex] (let [next-pos (get-random (get-neighbour pos [width height] mem))]
                                  (if (nil? next-pos)
                                    (let [ok-backtrack
                                            (take-first
                                              #(not (= 0 (count (get-neighbour % [width height] mem))))
                                              mem)]
                                      (if (nil? ok-backtrack)
                                        conex
                                        (make-maze width height ok-backtrack mem conex)))
                                    (make-maze width height next-pos
                                               (conj (conj mem pos) next-pos) (conj conex [pos next-pos]))))))

(defn get-connected-neighbours
  "Gets the connected neighbours of a node, pos, in maze dims with connections conex."
  [pos dims conex]
  (filter #(or (.contains conex [% pos]) (.contains conex [pos %])) (get-neighbour pos dims [])))

(defn solve-maze
  "Solves maze of dims dimensions represented by conex connections from start to end."
  ([conex dims start-pos end-pos] (solve-maze conex dims start-pos end-pos '() ))
  ([conex dims start-pos end-pos visiteds]
    (if (= start-pos end-pos)
      (list [start-pos end-pos])
      (let [next-node (take-first #(not (.contains visiteds %)) (get-connected-neighbours start-pos dims conex))]
        (if (nil? next-node)
          nil
          (let [rest-of-sol (solve-maze conex dims next-node end-pos (conj (conj visiteds next-node) start-pos))]
            (if (nil? rest-of-sol)
              (solve-maze conex dims start-pos end-pos (conj (conj visiteds start-pos) next-node))
              (conj rest-of-sol [start-pos next-node]))))))))


(defn get-drawn
  "Convert a matrix location to a screen one with given offset."
  [x offset]
  (* (+ 1 x) offset))

(defn get-disconnected-neighbours
  "Gets disconnected neighbours for a node, pos, within maze [width height] and connections conex."
  [pos [width height] conex]
  (filter #(not (or (.contains conex [pos %]) (.contains conex [% pos]))) (get-neighbour pos [width height] [])))

(defn draw-lines
  "Draws lines around a node [x y] with connections conex for a maze with dims to Graphics g."
  [[x y] dims conex offset #^Graphics g]
  (dorun (for [[xn yn] (get-disconnected-neighbours [x y] dims conex)]
           (if (= x xn)
             (. g drawLine (+ (get-drawn x offset) (/ offset 2))
               (+ (get-drawn y offset) (* (- yn y) (/ offset 2)))
               (- (get-drawn x offset) (/ offset 2))
               (+ (get-drawn y offset) (* (- yn y) (/ offset 2))))
             (. g drawLine (+ (get-drawn x offset) (* (- xn x) (/ offset 2)))
               (+ (get-drawn y offset) (/ offset 2))
               (+ (get-drawn x offset) (* (- xn x) (/ offset 2)))
               (- (get-drawn y offset) (/ offset 2)))))))

(defn make-panel
  "Makes a JPanel to contain a maze of dimensions given."
  [width height] (proxy [JPanel] []
                   (paintComponent [#^Graphics graphics]
                     (let [conex (make-maze width height)
                           offset 25
                           half-set (/ offset 2)]
                       (dorun (for [x (range width) y (range height)]
                                (draw-lines [x y] [width height] conex offset graphics)))
                       (. graphics drawLine half-set half-set half-set
                         (- (get-drawn height offset) half-set))
                       (. graphics drawLine half-set half-set
                         (- (get-drawn width offset) half-set) half-set)
                       (. graphics drawLine (- (get-drawn width offset) half-set) half-set
                         (- (get-drawn width offset) half-set)
                         (- (get-drawn height offset) half-set))
                       (. graphics drawLine half-set (- (get-drawn height offset) half-set)
                         (- (get-drawn width offset) half-set)
                         (- (get-drawn height offset) half-set))
                       (. graphics drawOval offset offset (/ offset 3) (/ offset 3))
                       (. graphics drawOval (get-drawn (- width 1) offset) (get-drawn (- height 1) offset)
                         (/ offset 3) (/ offset 3))
                       (. graphics setColor Color/RED)
                       (dorun (for [[[x1 y1] [x2 y2]] (solve-maze conex [width height]
                                                                  [0 0] [(- width 1) (- height 1)])]
                                (. graphics drawLine
                                  (get-drawn x1 offset)
                                  (get-drawn y1 offset)
                                  (get-drawn x2 offset)
                                  (get-drawn y2 offset))))))))


(defn run-maze
  "Makes a maze window with maze of given dims."
  [width height]
  (doto (JFrame. "Maze!")
    (.setSize 750 750)
    (.add (make-panel width height))
    (.setVisible true)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)))

(run-maze 25 25)
