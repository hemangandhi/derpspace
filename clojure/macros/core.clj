(ns macros.core)

(defmacro switch [v & cases]
  "Emulates java's switch statement.
  Everything must be a list of lists."
  (if (empty? cases) nil
    (if (or (= v (first cases)) (= :default (first cases)) (and (list? (first cases)) (.contains (first cases) v)))
      (let [[e# r#] (split-with #(not (= (last %) :break)) (take-nth 2 (rest cases)))
            body# (concat (apply concat e#) (remove #(= :break %) (first r#)))]
        `(do ~@body#))
      `(switch ~v ~@(drop 2 cases)))))

;(switch 1 1 ((println "1")) 2 ((println "2a") (println "2b")))
;prints from all the printlns.

(defmacro fn-pwr [fun pwr init]
  "Composes fun over itself pwr times. init contains the inital arguments.
  (fn-pwr inc 2 1) is 3."
  (if (= 0 pwr)
    init
    `(~fun (fn-pwr ~fun ~(dec pwr) ~init))))

;(println (fn-pwr inc 2 1))

(defmacro debug [body]
  (println body)
  (println "evaluates to")
  `(let [e# ~body]
    (println e#)
    e#))

;(println (debug (+ 1 3)))
