(ns clj-test.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn fact
  "Get the factorial of a number"
  [x]
  (if (<= x 0)
    1
    (* x (fact (- x 1)))))

(defn str-to-symbol-list [string]
  (map atom (.split string " ")))
