(ns util.curve
    (:require [quil.core :as q]))

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

  
