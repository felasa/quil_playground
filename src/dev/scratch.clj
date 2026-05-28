(ns dev.scratch
  (:import [fastnoise FastNoiseLite])
  (:require [quil.core :as q]
            [clojure.edn]
            [util.random :refer [simplex]]
            [util.curve :as curve]))

(def simplex-nt fastnoise.FastNoiseLite$NoiseType/OpenSimplex2)
(def super-simplex-nt fastnoise.FastNoiseLite$NoiseType/OpenSimplex2S)
(def perlin-nt fastnoise.FastNoiseLite$NoiseType/Perlin)
; Fractal types 
(def fbm-ft fastnoise.FastNoiseLite$FractalType/FBm)
(def none-ft fastnoise.FastNoiseLite$FractalType/None)

(defn gradient-sine-h [y width])
(defn gradien-linear-h [y ramp]
  (fn [x y] (/ y 800)))

; Organge Yellow HSB: 41, 92, 99
; combo 344 tiene azul negro amarillo negro
(def sunset-pal {:gold [41 92 99] :blue [] :black []})
(def palette
  (->> (clojure.edn/read-string (slurp "resources/sanzo-colors.edn"))
       :combos
       ;(take 10)))
       (filter #(= (:id_combo %) 344))
       first
       :hsb))
(def nclass (FastNoiseLite. 99999))
(.SetFractalType nclass fbm-ft)
(.SetFractalType nclass none-ft)
(.SetOctaves nclass 4)
(def nclass2 (FastNoiseLite. 666666))
(.SetFractalType nclass2 none-ft)
(.SetOctaves nclass2 4)
(def nclass3 (FastNoiseLite. 616263))
(.SetNoiseType nclass3 perlin-nt)
(.SetFractalType nclass2 none-ft)
(.SetOctaves nclass2 4)

(defn noise [nclass x y]
  (/ (+ 1 (.GetNoise nclass x y)) 2))

(defn setup [] (q/no-loop))
(defn draw []
  (q/color-mode :hsb 359 100 100)
  (apply q/background (palette 3))
  (doseq [x (range (q/width))
          y (range (q/height))]
    (let [n (noise nclass2 (* 25 x) (* 25 y))
          grad ((fn [x y] (- 1 (/ (Math/abs (- y 400)) 400))) x y)]
      (when (> grad n) (q/set-pixel x y (apply q/color (palette 1))))))
  (doseq [x (range (q/width)) y (range (q/height))]
    (let [n (noise nclass (* 25 x) (* 25 y))
          grad ((gradien-linear-h 800 1) x y)]
      (when (> grad n) (q/set-pixel x y (apply q/color [41 92 99])))))) ;(palette 0)))))))

(comment
  (q/sketch 
    :size [800 800]
    :setup setup
    :draw draw))

(defn os-field [x y]
  (let [r (/ (+ 1 (.GetNoise nclass x y)) 2)]
    [(Math/cos r) (Math/sin r)]))

(defn simplex-field [x y]
  (let [r (/ (+ 1 (simplex x y)) 2)]
    [(Math/cos r) (Math/sin r)]))

;(curve/field-curve simplex-field 10 10 [400 400])
;(curve/field-curve os-field 10 10 [400 400])

(reduce min (repeatedly 1e7 #(simplex (rand-int 800) (rand-int 800))))  
(comment 
  (q/sketch 
    :size [800 800]
    :setup setup
    :draw 
    (fn []
      (q/color-mode :rgb 1.0)
      (doseq [x (range (q/width)) y (range (q/height))]
        (let [v (/ (+ 1 (simplex (* 0.1 x) (* 0.1 y)) 2))]
          (q/set-pixel x y (q/color v)))))))

(comment 
  (q/sketch 
    :size [800 800]
    :setup setup
    :draw 
    (fn []
      (q/color-mode :rgb 1.0)
      (doseq [x (range (q/width)) y (range (q/height))]
        (let [v (q/noise (* 0.1 x) (* 0.1 y))]
          (q/set-pixel x y (q/color v)))))))

(comment 
  (q/sketch 
    :size [800 800]
    :setup setup
    :draw 
    (fn []
      (q/color-mode :rgb 1.0)
      (doseq [x (range (q/width)) y (range (q/height))]
        (let [v (noise nclass (* 23 x) (* 23 y))]
          (q/set-pixel x y (q/color (if (> 0.9 v) 0 1))))))))
(comment 
  (q/sketch 
    :size [800 800]
    :setup setup
    :draw 
    (fn []
      (q/color-mode :rgb 1.0)
      (doseq [x (range (q/width)) y (range (q/height))]
        (let [v (noise nclass3 (* 20 x) (* 20 y))]
          (q/set-pixel x y
                       (q/color (if (> 0.8 v) 1 0)))))))
  (q/sketch 
    :size [800 800]
    :setup setup
    :draw 
    (fn []
      (q/stroke 0 10)
      (dotimes [_ 10000]
        (let [p-ini [(rand-int 800) (rand-int 800)]
              crv (curve/field-curve simplex-field 3 10 p-ini)]
          (curve/draw-curve crv))))))
        
            

