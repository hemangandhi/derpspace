(ns bfk-gen.core
  (:import (javax.swing JOptionPane)))

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn gcd-list [list]
  (reduce gcd (first list) list))

(defn gen-str [string num]
  (if (= 1 num)
    string
    (str string (gen-str string (- num 1)))))

(defn gen-bfk [string]
  (let [inp-array (map int (.toCharArray string))
        inp-gcd (gcd-list inp-array)
        init+ (gen-str "+" inp-gcd)
        <-to-start (gen-str "<" (count string))
        final-> (gen-str ">." (count string))]
    (str init+ "[" (apply str (map #(str ">" %) (for [x inp-array] (gen-str "+" (/ x inp-gcd)))))
         <-to-start "-]" final->)))

(defn get-ui []
  (gen-bfk (JOptionPane/showInputDialog "Type in the string to convert to bfk code:")))

(get-ui)