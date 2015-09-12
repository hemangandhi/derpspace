(ns json-parse.core)

(defn string-split [st va ig-st ig-end ig-same]
  (loop [i 0 ins 0 lst-sub -1 lst '() init-ins 0]
    (if (= i (count st))
      (reverse (conj lst (.substring st (inc lst-sub) i)))
      (if (and (.contains va (.charAt st i)) (= 0 ins))
        (recur (inc i) ins i (conj lst (.substring st (inc lst-sub) i)) init-ins)
        (if (.contains ig-st (.charAt st i))
          (recur (inc i) (inc ins) lst-sub lst (if (= 0 init-ins) 1 0))
          (if (.contains ig-end (.charAt st i))
            (recur (inc i) (if (= ins 0) 0 (dec ins)) lst-sub lst (if (= 0 init-ins) 1 0))
            (if (.contains ig-same (.charAt st i))
              (recur (inc i) (if (= 0 (mod (+ init-ins ins) 2)) (inc ins) (dec ins)) lst-sub lst init-ins)
              (recur (inc i) ins lst-sub lst init-ins))))))))

(defn obj-to-vec [obj]
  (into [] (string-split obj [\: \,] [\[ \{] [\] \}] [\"])))

(defn arr-to-vec [arr]
  (into [] (string-split arr [\,] [\[ \{] [\] \}] [\"])))

(defn full-json-parse [json]
  (if (= (first json) \[)
    (map #(full-json-parse (.trim %)) (arr-to-vec (.substring json 1 (dec (count json)))))
    (if (= (first json) \{)
      (apply hash-map (map #(full-json-parse (.trim %)) (obj-to-vec (.substring json 1 (dec (count json))))))
      (read-string (.trim json)))))

(defn parse-file [path]
  (full-json-parse (.trim (.replaceAll (slurp path) "[\n\r]" ""))))

(get (first (parse-file "C:\\Users\\Heman\\Documents\\Torchlight\\web\\events.json")) "time")

