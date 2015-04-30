(ns online-problems.24-game)

(defn get-rand 
  "Gets random element of a collection or nil for empty collections."
  [coll]
  (if (or (not (coll? coll)) (= 0 (count coll)))
    nil
    (nth coll (int (* (count coll) (java.lang.Math/random))))))

(defn take-last-n
  "Gives the last n items of coll in order."
  ([n coll] (if (< 0 n) (take-last-n (- n 1) (butlast coll) (list (last coll)))))
  ([n coll mem] (if (< 0 n) (take-last-n (- n 1) (butlast coll) (conj mem (last coll))) mem)))

(defn gen-24 [target len max &{:keys [ops so-far] :or {ops '(+ - / *)
                                                       so-far nil}}]
  "Generates a clojure statement that is true and has operations (specified in ops) performed
   on target to get a specific value."
 (if (< 1 len)
   (let [op (get-rand ops)
         tar (case op
               / (get-rand (for [x (range 1 target)
                                 :when (= 0 (mod target x))] x))
               - (get-rand (range (min target max)))
               * (get-rand (range 1 max))
               (get-rand (range max)))]
               (gen-24 (apply op [target tar])
                       (- len 1) max :ops ops 
                       :so-far (list op (if (nil? so-far) target so-far) tar)))
   (list '= so-far (eval so-far))))

(defn get-nums [target len max &{:keys [ops] :or {ops '(+ - * /)}}]
  "Gets 4 numbers for the use of the popular game 24."
  (take-last-n 4 (flatten (gen-24 target len max :ops ops))))
  
               