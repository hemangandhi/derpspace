(ns wrappers.core)

(defmacro def-wrapped [name wrapper args & body]
  (let [sym (gensym)]
    `(def ~name (fn [& ~sym] (apply (~wrapper (fn [~@args] ~@body)) ~sym)))))

(defn auto-partial [param-cts]
  (fn [f]
    (letfn [(inner [& a]
              (if (.contains param-cts (count a))
                (if (= (last param-cts) (count a))
                  (apply f a)
                  (fn [& b]
                    (apply inner (concat a b))))
                nil))]
      inner)))

(def-wrapped w-sum (auto-partial [1 2 3]) [a b c]
  (+ a b c))

(((w-sum 1) 2) 3)
(w-sum 1 2 3)
((w-sum 1) 2 3)
((w-sum 1 2) 3)


