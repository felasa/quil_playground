(ns util.curve
  (:require [util.fields :as fields]
            [clojure.math :as math]
            [quil.core :as q]))

(defn field-curve
  [field-fn segmen-len n-segments start-p]
  (loop [segments 1
         [x y :as p] start-p
         return [[x y]]]
    (if (<= segments n-segments)
      (let [[dx dy :as delta] (field-fn x y)
            [x' y'] (mapv + p (map #(* segmen-len %) delta))]
        (recur (inc segments) [x' y'] (conj return [x' y'])))
      return)))

(defn draw-curve
  [pts] 
  (doseq [pair (partition 2 1 pts)]
    (apply q/line (flatten pair))))

(defn draw-shape
  [vxs]
  (q/begin-shape)
  (doseq [vx vxs]
    (apply q/vertex vx))
  (q/end-shape))

  
