(ns macros.core)

(defmacro switch [v & cases]
  (if (empty? cases) nil
    (if (or (= v (first cases)) (= :default (first cases)) (and (list? (first cases)) (.contains (first cases) v)))
      (let [[e# r#] (split-with #(not (= (last %) :break)) (take-nth 2 (rest cases)))
            body# (concat e# (remove #(= :break %) (first r#)))]
        `(do ~@ body#))
      `(switch ~v ~@(drop 2 cases)))))

(switch 2
        8 ((println "wat") :break)
        1 (println "fall")
        2 (println "down")
        :default (println "to the ground"))
