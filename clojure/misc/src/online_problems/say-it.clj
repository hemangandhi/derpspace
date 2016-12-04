
(def say-it (letfn [(f [wds] (fn ([] (clojure.string/join " " wds))
                                 ([wd] (f (conj wds wd)))))]
              (f [])))
