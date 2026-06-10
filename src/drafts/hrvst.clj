(ns drafts.hrvst
  (:require [quil.core :as q]
            [util.core :refer [w h vector-scale vector-add point-lerp]]))

(defn harvest [radius position]
  (q/color-mode :hsb 359 100 100 1.0)
  (q/no-fill)
  ;(q/curve-tightness 0)
  (q/ellipse-mode :radius)
  (q/stroke-weight 2)
  ;(q/ellipse (w 2) (h 2) 200 200)
  (doseq [;i (range 800)]
          angle (range 0 (* 2 Math/PI) 0.07)]
    (let [[xini yini :as p-ini] position
          ;angle (* 2 (rand) Math/PI)
          [dx dy :as v] [(Math/cos angle) (Math/sin angle)]
          [xend yend :as p-end]
          (vector-add (vector-scale radius v) position [(* 5 (q/random-gaussian)) (* 5 (q/random-gaussian))])
          ts (sort (repeatedly 2 rand))
          mpoints (mapv #(point-lerp p-ini p-end %) ts)
          cpoints (mapv #(vector-add % (vector-scale (* (rand-nth [-1 1]) (/ 7 20) radius) [(- dy) dx])) mpoints)]
      (q/no-fill)
      (q/stroke 0 0 100 0.2)
      (apply q/bezier (flatten (concat p-ini cpoints p-end)))
      ;(q/begin-shape)
      ;(q/curve-vertex xini yini)
      ;(q/curve-vertex xini yini)
      ;(apply q/curve-vertex (cpoints 0))
      ;(apply q/curve-vertex (cpoints 1))
      ;(q/curve-vertex xend yend)
      ;(q/curve-vertex xend yend)
      ;(q/end-shape)
      (q/fill 30 50 (rand-int 100) 1)
      (q/no-stroke)
      (q/ellipse xend yend (* radius (/ 3 200)) (* radius (/ 3 200)))))) 
      ;(q/line (w 2) (h 2) xend yend))))

(q/defsketch sketch 
  :size [800 800]
  :setup (fn [] (q/no-loop) (q/background 0))
  :settings #(q/smooth 16)
  :draw #(do 
           (harvest 300 [350 350])
           (harvest 100 [700 700])))
