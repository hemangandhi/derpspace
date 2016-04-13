(ns fsm.core
  (:gen-class))

(defn node-to-fn [node alphabet fsm]
  (let [im (fsm node)
        cases (apply concat (for [x alphabet] 
                              (if (contains? im x)
                                [x `(~(symbol (str (im x) "-fn")) 
                                              (rest string))]
                                [x `(~(symbol (str node "-fn"))
                                              (rest string))])))]
    `(fn [string]
       (if (empty? string)
         ~node
         (condp = (first string) ~@cases)))))


(defn fsm-to-clj [name-of alphabet fsm st-node fin-sts]
  (let [st-sym (symbol "st")
        all-nodes (apply concat 
                    (map #(vec 
                            [(symbol (str % "-fn")) 
                             (node-to-fn % alphabet fsm)]) 
                         (keys fsm)))]
    `(defn ~(symbol name-of) [~st-sym]
       (letfn [~@all-nodes]
                  (contains? ~fin-sts 
                             (~(symbol (str st-node "-fn")) ~st-sym))))))

(defmacro cf-to-macro [cf macro-name nargs]
  (let [ags# (for [x (range nargs)] (symbol (str (char (+ 65 x)))))]
  `(defmacro ~(symbol macro-name) [~@ags#]
     (~cf ~@ags#))))

(cf-to-macro fsm-to-clj "make-fsm" 5)
