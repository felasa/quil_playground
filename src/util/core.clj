(ns util.core)

(defn norm [p]
  (Math/sqrt (reduce + (map #(* % %) p))))

(defn vector-add
  [p1 p2]
  (mapv + p1 p2))

(defn dot [p1 p2]
  (mapv * p1 p2))
