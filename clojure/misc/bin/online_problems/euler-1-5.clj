(ns online-problems.euler-1-5)

(defn euler-1 [num]
  (cond
    (= 0 num) 0
    (or (= 0 (mod num 3))
        (= 0 (mod num 5)))
    (+ num (euler-1 (- num 1)))
    :else (euler-1 (- num 1))))

(defn fib [n]
  (if (<= n 2) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn euler-2 []
  (let [max 4000001]
    (apply +
    (for [x (map fib (range max)) :when (and (<= x max) (even? x))]
      x))))

(defn euler-3
  ([num] (euler-3 num 2))
  ([num fact] (if (= 0 (mod num fact)) (/ num fact) (euler-3 num (+ 1 fact)))))

(defn palindrome? [string]
  (= (seq string) (reverse string)))

(defn euler-4 [num1 num2]
  (cond 
    (= 100 num1) (* num1 num2)
    (= 100 num2) (euler-4 (- num1 1) 999)
    (palindrome? (str (* num1 num2))) (* num1 num2)
    :else (euler-4 num1 (- num2 1))))

(defn lcm
  ([list-arg] (reduce lcm (first list-arg) list-arg))
  ([a b] (if (= 0 (mod a b)) a (lcm a b (+ a a))))
  ([a b ctr] (if (= 0 (mod ctr b)) ctr (lcm a b (+ ctr a)))))

(defn euler-5 [] (lcm (for [x (range 1 21)] x)))