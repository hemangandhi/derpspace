(ns json-parse.core)

(defn string-split [st va ig-map]
  (loop [i 0 p '() r [-1]]
    (cond
      (>= i (count st)) (let [nr (conj r (count st))]
                          (filter #(< 0 (count %)) 
                                  (for [j (range 1 (count nr))] 
                                    (subs st (inc (nth nr (dec j))) (nth nr j)))))
      (and (empty? p) (.contains va (nth st i))) (recur (inc i) p (conj r i))
      (and (.contains (vals ig-map) (nth st i)) 
           (= (nth st i) (ig-map (first p)))) (recur (inc i) (rest p) r)
      (contains? ig-map (nth st i)) (recur (inc i) (conj p (nth st i)) r)
      :else (recur (inc i) p r))))

(defn json-obj-to-map [obj-str]
  (apply hash-map (string-split obj-str #{\: \, \space} {\' \', \" \", \[ \], \{ \}})))

(defn json-arr-to-vec [arr-str]
  (vec (string-split arr-str #{, \space} {\' \', \" \", \[ \], \{ \}})))

(defn full-parse [st]
  (condp = (first st)
    \[ (map full-parse (json-arr-to-vec (subs st 1 (dec (count st)))))
    \{ (let [mp (json-obj-to-map (subs st 1 (dec (count st))))]
         (into {} (for [[k v] mp] [(full-parse k) (full-parse v)])))
    (read-string st)))

(println (full-parse "{\"name\" : \"Price, Elder\", \"age\": 39, \"wives\": [1, 2, 3]}"))
