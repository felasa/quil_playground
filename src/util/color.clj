(ns util.color)

(defn hex-to-rgb
  [hex]
  (mapv read-string 
       (map #(reduce str "0x" %) 
            (partition 2 (subs hex 1)))))
