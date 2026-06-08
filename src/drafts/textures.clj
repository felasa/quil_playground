;; paper texture from: https://sighack.com/post/generative-watercolor-in-processing
(ns drafts.textures
  (:require [quil.core :as q]
            [genartlib.poisson-disc :as pois]
            [util.core :as util :refer [w h]])
  (:import [fastnoise FastNoiseLite]))

(declare sketch)

(defn setup [] 
  (q/no-loop))

(defn draw []
  (q/no-stroke)
  (q/fill 0)
  ;(q/ellipse-mode :radius)
  (let [ps (pois/poisson-disc-sample 10 4 100 700 100 700)]
    (doseq [[x y] ps]
      (q/ellipse x y 3 3))))
  
(defn custom-line 
  [[x1 y1] [x2 y2]]
  (if (<= x1 x2)
    (let [step 1
          slope (float (/ (- y2 y1) (- x2 x1)))
          nscale 3e2]
      (loop [x x1 y y1]
        (when (<= x x2)
          (let [x' (+ x step) y' (+ y (* step slope))
                sx (* nscale x) sy (* nscale y)]
            ;(println x' y')
            (q/stroke-weight (+ 2 (q/map-range (q/noise sx sy) 0 1 -0.5 0.5)))
            (q/line x y
                    ;x' y') 
                    (+ x' (q/map-range (q/noise sx sy) 0 1 -1 1))
                    (+ y' (q/map-range (q/noise sx sy) 0 1 -1 1)))
            (recur x' y')))))
    (let [step 1
          slope (float (/ (- y2 y1) (- x2 x1)))
          nscale 3e2]
      (loop [x x2 y y2]
        (when (<= x x1)
          (let [x' (+ x step) y' (+ y (* step slope))
                sx (* nscale x) sy (* nscale y)]
            ;(println x' y')
            (q/stroke-weight (+ 2 (q/map-range (q/noise sx sy) 0 1 -0.5 0.5)))
            (q/line x y
                    ;x' y') 
                    (+ x' (q/map-range (q/noise sx sy) 0 1 -1 1))
                    (+ y' (q/map-range (q/noise sx sy) 0 1 -1 1)))
            (recur x' y')))))))

(defn grid
  [spacing]
  (q/color-mode :rgb 1.0  1.0 1.0 1.0)
  (q/background 0.1)
  (doseq [i (range (- (w)) (+ (w) (h)) spacing)]
    (q/stroke 1 (* 0.05 (rand)))
    (custom-line [i 0] [(+ i (h)) (h)]))
  (doseq [i (range (+ (w) (h)) (- (w)) (- spacing))]
    (q/stroke 1 (* 0.01 (rand)))
    (custom-line [i 0] [(- i (h)) (h)]))
  (q/fill 1 0 0 0.3)
  (q/ellipse 400 400 100 100))

(q/defsketch sketch 
  :size [800 800]
  :setup setup
  :draw #(grid 5))
 
