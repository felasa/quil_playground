(ns sketch.sinkflow
  (:require [quil.core :as q]
            [generative.flowfields :refer [draw-curve-with-field]]
            [util.fields :as fields]))

;; same as in util.fields but dont normalize
;; without normalizing it allows for different velocities but the vectors end up
;;  tiny.
;; when using perlin-field its normalized because its cos,sin of angle but the curl
;; field has different velocities. 
;; unsire whcih is better/more useful, have to play around
(defn curl-field
  "Return a vector at [x y] representing the curl field for noise at that point."
  [scale]
  (fn [x y] 
    (let [delta 1
          dfy0 ((fields/perlin-noise scale) x (- y delta))
          dfy1 ((fields/perlin-noise scale) x (+ y delta))
          dfx0 ((fields/perlin-noise scale) (- x delta) y)
          dfx1 ((fields/perlin-noise scale) (+ x delta) y)
          [vx vy] [(/ (- dfy1 dfy0) (* 2 delta))
                   (/ (- dfx0 dfx1) (* 2 delta))]
          N (q/mag vx vy)]
      ;[vx vy]))) 
      (mapv #(/ % N) [vx vy]))))
      
(def pallete [[0xcc 0x12 0x36] [0xfd 0xbe 0x68] [0x00 0x97 0x8c]])

;; params to play around with: scale for curl field, dampening of sink vector field 
;;  and also its scale
(defn sink 
  "tries to get a field to converge to the center. ideally when further there's more
   variation but near the center it draws closer"
  []
  (q/no-loop)
  (q/background 50)
  (q/noise-seed (System/nanoTime))
  (q/stroke 250)
  (dotimes [i 500]
    ;(apply q/stroke (rand-nth pallete))
    (draw-curve-with-field 
      (fields/avg-fields 
        (comp (fn [V] (mapv #(* 3 %) V)) (curl-field 0.005))
        (fn [x y]
          (let [d (q/dist x y (/ (q/width) 4) (/ (q/height) 4))] 
            (->> [x y]
                 (map - [(/ (q/width) 4) (/ (q/height) 4)])
                 (map #(* (/ d) 3 %)))))
        (fn [x y]
         (let [d (q/dist x y (* (q/width) (/ 3 4)) (* (q/height) (/ 3 4)))] 
           (->> [x y]
                (map - [(* (q/width) (/ 3 4)) (* (q/height) (/ 3 4))])
                (map #(* (/ d) 3 %))))))   
      q/line
      200
      5
      (rand-int (q/width))
      (rand-int (q/height)))))

(declare sk-sink)
(q/defsketch sk-sink
  :title "Sink"
  :size [900 900]
  :settings #(q/smooth 8)
  :setup #(do (q/background 50))
  :draw sink)

