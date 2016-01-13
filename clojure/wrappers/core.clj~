(ns wrappers.core)

(defmacro def-wrapped [name wrapper args & body]
  (let [sym (gensym)]
    `(def ~name (fn [& ~sym] (apply (~wrapper (fn [~@args] ~@body)) ~sym)))))

(defn partial-til [con & {:keys [err] :or {err (fn [a] false)}}]
  (fn [f]
    (letfn [(inner [& a]
              (cond
                (err a) nil
                (con a) (apply f a)
                :else (fn [& b]
                        (apply inner (concat a b)))))]
      inner)))

(defn auto-partial [param-cts]
  (partial-til (fn [a] (= (count a) (last param-cts)))
    :err (fn [a] (not (.contains param-cts (count a))))))

(def-wrapped w-sum (auto-partial [1 2 3]) [a b c]
  (+ a b c))

(((w-sum 1) 2) 3)
(w-sum 1 2 3)
((w-sum 1) 2 3)
((w-sum 1 2) 3)


