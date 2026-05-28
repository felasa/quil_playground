(ns dev.textures
  (:require [quil.core :as q]
            [genartlib.poisson-disc :as pois])
  (:import [java FastNoiseLite]))

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
  
(q/defsketch sketch 
  :size [800 800]
  :setup setup
  :draw draw)
 
