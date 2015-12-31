(ns group-theory.core)

(defn check-group [op va]
  (let [iden-elem (first (filter #(and (= (op % (first va)) (first va)) (= (op (first va) %) (first va))) va))]
    (and (not (nil? iden-elem))
         (every? identity (for [x va y va z va] (= (op x (op y z)) (op (op x y) z))))
         (every? #(some identity (for [x va] (and (= (op x %) iden-elem) (= (op % x) iden-elem)))) va))))

(def dummy [0 1 2 3])

(defn dummy-op [x y]
  (mod (+ x y) 4))

(check-group dummy-op dummy)
