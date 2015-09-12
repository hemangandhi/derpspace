(ns data-algebra
  (:use clojure.set))

;[yin yang] via first, last.

(defn rem-undef [s]
  (into #{} (remove #(= :undefined %) s)))

(defn transpose [[yin yang]]
  [yang yin])

(defn yin-rel [r]
  (into #{} (map first r)))

(defn yang-rel [r]
  (into #{} (map last r)))

(defn transpose-rel [r]
  (into #{} (map transpose r)))

(defn yin-functional? [r]
  (= (count r) (count (yin-rel r))))

(defn yang-functional? [r]
  (yin-functional? (transpose-rel r)))

(defn compose [[yin1 yang1] [yin2 yang2]]
   (if (= yang1 yin2)
     [yin1 yang2]
     :undefined))

(defn cross [r1 r2 fun]
  (into #{} (for [a r1 b r2]
              (fun a b))))

(defn compose-rels [rel1 rel2]
  (rem-undef (cross rel1 rel2 compose)))

(defn cross-union [r1 r2]
  (cross r1 r2 union))

(defn cross-intersection [r1 r2]
  (cross r1 r2 intersection))

(defn yin-clan [c]
  (apply union (map yin-rel c)))

(defn yang-clan [c]
  (apply union (map yang-rel c)))

(defn compose-clans [c1 c2]
  (into #{} (cross c1 c2 compose-rels)))

(defn super-restrict [r1 r2]
  (if (superset? r1 r2)
    r1 :undefined))

(defn sub-restrict [r1 r2]
  (if (subset? r1 r2)
    r1 :undefined))

(defn super-restrict-clans [c1 c2]
  (rem-undef (cross c1 c2 super-restrict)))

(defn sub-restrict-clans [c1 c2]
  (rem-undef (cross c1 c2 sub-restrict)))

;some tests

(def my-data #{[1 2]})
