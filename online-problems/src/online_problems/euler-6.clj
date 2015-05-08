(ns online-problems.euler-6)

(defn euler-6 [x] (- (#(* % %) (apply + (range x))) 
  (apply + (map #(* % %) (range x)))))

(defn all-+? [list] (if (empty? list) true (and (< 0 (first list)) (all-+? (rest list)))))

(defn n-primes ([n] (n-primes [2] n))
 ([primes n] (if (= 0 n) 
               primes 
               (n-primes (conj primes 
                         (let [np-a (atom (+ 1 (last primes)))]
                           (while (not= 0 (count (filter #(= 0 (mod @np-a %)) primes))) 
                             (do (swap! np-a inc)))
                          @np-a)) 
                         (- n 1)))))

(defn str-to-digits [string] (map #(java.lang.Integer/parseInt (str % "")) (.toCharArray string)))

(defn greatest-consec-prod "Solves Euler 8." [digits n] 
  (if (empty? digits) 0
    (java.lang.Math/max (apply * (for [x (range n) :when (< x (count digits))] (nth digits x)))
                        (greatest-consec-prod (rest digits) n))))
 
(defn euler9 [sum] (apply * (first (filter (fn [[a b c]] (= sum (+ a b c))) (for [a (range 1 sum)
                                                                                  b (range 1 sum)
                                                                                  c (range 1 sum)
                                                                                  :when (= (* c c) (+ (* a a) (* b b)))]
                                                                              [a b c])))))

(defn triangle-num [n] (* n (+ 1 n) 1/2))

(defn count-factors [n] (count (for [i (range 1 (+ 1 n)) :when (= 0 (mod n i))] i)))

(defn euler12 [n] (let [i (atom 1E100)] (while (>= (count-factors (triangle-num @i)) n) (do (swap! i dec))) 
                    (triangle-num @i)))

(defn eulers-meth 
  "Performs a euler approximation of dy/dx(x,y)."
  [dydx xi xf yi steps]
  (if (= xi xf) yi
    (eulers-meth dydx (+ xi (/ (- xf xi) steps)) xf
                 (+ yi (* (dydx xi yi) (/ (- xf xi) steps)))
                 steps)))
