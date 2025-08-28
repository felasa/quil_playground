(ns sketch.color-transition
  (:require [quil.core :as q]))

(declare sketch)

(defn draw [color1 color2]
  (q/color-mode :hsb 359 100 100 1.0)
  (doseq [x (range (q/width)) y (range (q/height))]
    (q/set-pixel x y (q/lerp-color (apply q/color color1) (apply q/color color2) (/ y (q/height)))))
  (q/quad 100 100 100 700 700 700 700 100)
  (q/fill 0)
  (q/quad 200 200 200 600 600 600 600 200)
  (doseq [x (range (q/width)) y (range (q/height))]
    (when (< (q/mag x y) 600)
      (let [pxl (q/get-pixel x y)]
        (q/set-pixel x y (q/color (bit-xor 0xFFFFFF pxl))))))) 

(q/defsketch sketch 
  :setup (fn [] (q/no-loop))
  :size [800 800]
  :draw #(draw [240 100 50] [0 100 50]))
