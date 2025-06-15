(ns util.core)

(defmacro local-context []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols ) symbols)))

(defn hex-to-rgb
  [hex]
  (mapv read-string 
       (map #(reduce str "0x" %) 
            (partition 2 (subs hex 1)))))


