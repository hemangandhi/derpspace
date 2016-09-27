(ns tree-draw.core
  (:import [javax.swing JPanel JFrame]
           [java.awt Graphics Color]))

(defn set-union [set1 set2]
  (reduce conj set1 set2))

(defn lists->point-map 
  ([lists offset width] (lists->point-map lists offset width 1 0 nil))
  ([lists offset width level x-offset par-pt]
   (let [new-width (/ width (dec (count lists)))
         new-x-offset #(+ x-offset (* % new-width))
         mk-pt (fn [width x-offset level] [(+ x-offset (/ width 2)) (* level offset)])
         recursion (fn [idx new-val par-pt] (if (coll? new-val) 
                                              (lists->point-map new-val 
                                                                offset 
                                                                new-width
                                                                (inc level) 
                                                                (new-x-offset idx)
                                                                par-pt)
                                              {:points {(mk-pt new-width (new-x-offset idx) (inc level)) new-val}
                                               :lines #{[par-pt (mk-pt new-width (new-x-offset idx) (inc level))]}}))
         new-par-pt (mk-pt width x-offset level)
         acc-lines (if (nil? par-pt) #{} #{[par-pt new-par-pt]})]
     (first (reduce (fn [[acc idx] new-val]
                      (let [rec (recursion idx new-val new-par-pt)]
                        [(-> acc
                             (assoc :points (merge-with (fn [o n] n)
                                                        (:points rec)
                                                        (:points acc)))
                             (assoc :lines (set-union (:lines rec) (:lines acc))))
                         (inc idx)]))
                    [{:points {new-par-pt (first lists)}
                      :lines acc-lines} 0]
                    (rest lists))))))

(defn get-box [graphics str x y & {:keys [y-pad x-pad] :or {y-pad 1 x-pad 1}}]
  (let [font-metr (. graphics getFontMetrics)
        strlen (. font-metr stringWidth str)
        strh (. font-metr getHeight)]
    {:box [x (- y strh) (+ strlen x-pad) (+ strh y-pad)]
     :center [(+ x (/ strlen 2)) (- y (/ strh 2))]}))

(defn draw-white-rect [graphics x1 y1 width height]
  (. graphics drawRect x1 y1 width height)
  (. graphics setColor Color/WHITE)
  (. graphics fillRect x1 y1 width height)
  (. graphics setColor Color/BLACK))

(defn render-tree [#^Graphics graphics tree offset width]
  (let [pts (lists->point-map tree offset width)
        boxes (into {} (map (fn [[[x y] txt]] 
                              [[x y] (get-box graphics (str txt) x y)]) (:points pts)))
        pt->center #(get-in boxes [% :center])]
    (dorun (map (fn [[[o-x1 o-y1] [o-x2 o-y2]]]
                  (let [[x1 y1] (pt->center [o-x1 o-y1])
                        [x2 y2] (pt->center [o-x2 o-y2])]
                    (. graphics drawLine x1 y1 x2 y2))) 
                (:lines pts)))
    (dorun (map (fn [[[x y] txt]]
                  (apply draw-white-rect (concat [graphics] (get-in boxes [[x y] :box])))
                  (. graphics drawString (String. (str txt)) (int x) (int y))) 
                (:points pts)))))

(defn make-frame [tree offset width]
  (doto (JFrame. "Tree render")
    (.setSize width width)
    (.setVisible true)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.add (proxy [JPanel] []
             (paintComponent [#^Graphics graphics]
               (render-tree graphics tree offset width))))))

(defn prob-2-1-l []
  (make-frame '(<start> (<exp>
                          (<exp> (<var> a))
                          V
                          (<exp>
                            (<exp> (<const> false))
                            \^
                            (<exp>
                              (<exp> (<var> b))
                              "->"
                              (<exp> (<const> true)))))) 30 750))

(defn prob-2-1-r []
  (make-frame '(<start> (<exp>
                               (<exp> 
                                 (<exp> 
                                   (<exp> (<var> a)) 
                                   V 
                                   (<exp> (<const> false))) 
                                 \^ 
                                 (<exp> (<var> b))) 
                          "->" (<exp> (<const> true)))) 30 750))

(defn prob-2-2-l []
  (make-frame '(V a (\^ false ("->" b true))) 30 450))

(defn prob-2-2-r []
  (make-frame '("->" (\^ (V a false) b) true) 30 450))

(defn prob-2-5-parse []
  (make-frame '(<start> 
                 (<cond-expr> 
                   (<or-expr> 
                     (<and-expr> 
                       (<non> (<var> a))) 
                     V
                     (<and-expr> (<non> (<co> true))
                                 \^
                                 (<and-expr> (<non> (<var> b))))) 
                   -> (<cond-expr> (<or-expr> (<and-expr> (<non> (<co> false))) 
                                              V 
                                              (<and-expr> (<non> (<co> true)))))))
              30 650))

(defn prob-2-5-ast []
  (make-frame '(-> (V a (\^ true b)) (V false true)) 30 450))
