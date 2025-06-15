(ns generative.watercolor
  "Tries to implement https://www.tylerxhobbs.com/words/a-guide-to-simulating-watercolor-paint-with-generative-art"
  (:require 
      [quil.core :as q]
;      [clojure.math :as math]
;      [clojure.data.json :as json]
      [shapes.polygons :as poly]
      [util.transform :as util]))

(defn blob
  [mutations scale std annealing]
  (let [base-path (poly/n-gon scale 8)
        path (into base-path (take 2 base-path))]
    (loop [
           return path
           mutated 0
           std std]
      (if (<= mutated mutations)
        (recur (util/mutate-path-v std return)
               (inc mutated)
               (* std annealing))
        return))))

(defn draw-blob
  ;mut 4-6
  [scale mutations layers color position]
  (q/stroke-weight 0)
  (apply q/translate position)
  (apply q/fill color) 
  (dotimes [i layers]
    (let [shape (blob mutations scale 30 0.7)]
      (q/begin-shape)
      (doseq [vxs (partition 2 shape)]
        (apply q/vertex vxs))
      (q/end-shape)))
  (apply q/translate (map - position)))

(defn draw []
  ;(q/background 200)
  (q/no-stroke)
  (draw-blob 800 5 10 [255 255 255 4] [400 300])
  (draw-blob 260 5 15 [255 128 100 8] [500 200])
  (draw-blob 400 5 15 [93 12 200 8] [101 300])
  (draw-blob 400 5 15 [93 12 200 8] [100 500])
  (draw-blob 250 5 15 [255 128 100 8] [400 300])
  (draw-blob 800 5 10 [255 255 255 4] [400 300])
  (draw-blob 400 5 15 [93 12 200 8] [101 400])
  (draw-blob 400 5 15 [93 12 200 8] [101 100])
  (draw-blob 800 5 10 [255 255 255 4] [400 300])
  (draw-blob 250 5 15 [255 128 100 8] [400 300])
  (draw-blob 400 5 15 [93 12 200 8] [101 300])
  (draw-blob 240 5 15 [255 128 100 8] [200 250]))

(defn setup []
  (q/no-loop))

(comment 
 (q/defsketch sketch
   :title "Watercolor"
   :display 1
   :settings #(q/smooth)
   :setup setup
   :draw #(draw);blob-mask;#(draw-splat 50 5) 
   :size [800 600]
   :features [:resizable]
   :renderer :java2d))

(comment 
  (quil.applet/with-applet
    generative.watercolor/sketch 
    (q/save (str "out/watercolor_" (subs (str (random-uuid)) 0 5) ".png")))) ;
