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
                