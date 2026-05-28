(ns sketch.testing
  (:require [quil.applet]
            [quil.core :as q]
            [util.transform :as t]
            [util.curve :refer [draw-shape]]
            [util.fields :as f]))

(defn draw []
  (q/no-stroke)
  (q/fill 38 16 50)
  (let [square [[200 200] [400 200] [400 400] [200 400] [200 200]]
        mutated (t/mutate-path-with-fn (f/perlin-field 0.005) 10 [20] square)]
    (draw-shape mutated)))

(declare testsketch)
(q/defsketch testsketch
  :size [800 800]
  :setup (fn [] (q/no-loop))
  :draw draw) 
  
(quil.applet/with-applet sketch.testing/testsketch
  (let [[p1 p2] [[1 1] [2 2]]]
   (let [[x1 y1] p1 [x2 y2] p2
         [xm ym] (map #(/ % 2.0) (mapv + p1 p2))]
     ;(((f/perlin-field 0.005) xm ym)))))
     (t/mutate-segment-with-fn (f/perlin-field 0.01) p1 p2))))
  
