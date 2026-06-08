(ns sketches.triangle-gradients
  (:require [quil.core :as q]
            [util.core :as util :refer [w h]]
            [util.random :as rnd]))

(defn midpoint [p1 p2]
  (util/vector-scale 0.5 (map + p1 p2)))

(defn bound [min-v max-v v]
  (min (max v min-v) max-v))

(def rnd (java.util.Random.))

(defn split-triangle
  [[p1 p2 p3]]
  ;pick a side (longest?)
  (let [d1 (util/dist p1 p2)
        d2 (util/dist p2 p3)
        d3 (util/dist p3 p1)
        max-len (max d1 d2 d3)
        [o1 o2 o3]
        (cond 
          (== max-len d1) [p1 p2 p3]
          (== max-len d2) [p2 p3 p1]
          (== max-len d3) [p3 p1 p2])
        ;move the parens after noise for wrong but cool split
        ;mp (mapv #(Math/round (+ (* 0.5 (q/random-gaussian)) %)) (midpoint o1 o2))]
        mp (util/point-lerp o1 o2 (+ 0.5 (bound 0.1 0.9 (* 0.5 (.nextGaussian rnd)))))]
    [[o1 mp o3] [o2 mp o3]]))

(comment 
  (mapcat split-triangle (split-triangle [[0 0] [100 0] [100 100]])))
    
  ;random point on side
  ;create triangles with point as vertex

(defn rec-split
  [max-iters prob triangles]
  (loop [iter 1
         return triangles]
    (if (<= iter max-iters)
      (let [t (peek return)
            ts (pop return)
            split (if (> prob (rand)) (split-triangle t) [t])]
        ;; we can remove shuffle to iterate over starting triangles, appending new ones last
        (recur (inc iter) (shuffle (into split ts))))
      return)))

;; try doing one by one to allow odds of splitting
(defn recursive-splitting
  "triangle a trio of points"
  [iters triangles]
  (loop [i 1 triangles triangles]
    (if (<= i iters)
      (recur (inc i) (mapcat split-triangle triangles))
      triangles)))
  

(defn draw-split [iters prob]
  (q/no-stroke)
  ;(q/stroke 255 0 0)
  (q/fill 10)
  (let [clr (atom 10)
        t [[[0 0] [800 0] [800 800]]
           [[0 0] [0 800] [800 800]]]
        ts (rec-split iters prob t)]
        ;ts (recursive-splitting iters t)]
    (doseq [[[x1 y1] [x2 y2] [x3 y3] :as T] ts]
      (q/fill @clr)
      (swap! clr #(+ % 10))
      (q/triangle x1 y1 x2 y2 x3 y3))))
      ;(apply q/triangle (flatten T)))))
    

(defn setup [] (q/no-loop))

(defn draw [angle]
  (q/no-stroke)
  (q/color-mode :rgb 1.0)
  (q/rect 200 200 400 400)
  (doseq [x (range (q/width)) y (range (q/height))]
    (when (and (< 200 x 600) (< 200 y 600))
      (let [scale 0.03
            g ((util/angle-gradient angle [200 200] [600 600]) [x y])
            n (q/noise (/ x scale) (/ y scale))]
        (when (> n g) (q/set-pixel x y (q/color 0)))))))


(def pals {:bpy [[183 100 61] [232 40 42] [64 24 77]]
           :tbd [[182 43 82] [198 94 44]]
           :red [[337 85 49] [344 80 65]]
           :green [[118, 24, 58] [173, 100, 31] [108, 25, 33]]
           :gray [[199, 32, 30] [73, 6, 64] [167, 8, 69]]
           :oby [[14, 85, 92] [221, 88, 19] [57, 100, 100]]})
           
;; variations:
;; -- gradient is color (pixel over background)
;; -- gradient is light/shade (black/white pixel over triange fill)
;; -- i forgot third
;; -- dither as value
;; -- add light/shade as a separate transparent layer
(defn draw-rnd-grad [variation iters pal]
  (q/background 40)
  (q/no-stroke)
  (q/color-mode :hsb 359 100 100 1.0)
  (let [;t [[[400 0] [00 800] [800 800]]]
        t [[[0 0] [800 0] [0 800]] [[800 800] [800 0] [0 800]]]
        ;t [[[0 400] [400 0] [800 400]] [[0 400]   [400 800] [800 400]]
        ;   [[0 0]   [0 400] [400 0]]   [[400 0]   [800 0] [800 400]]
        ;   [[0 400] [0 800] [400 800]] [[800 400] [800 800] [400 800]]
        ts (rec-split iters 1.5 t)]
        ;ts (recursive-splitting iters t)]
        ;pal [[183 100 61] [232 40 42] [64 24 77]]]
    (doseq [T ts]
      (let [min-x (->> T (map #(% 0)) (reduce min))
            min-y (->> T (map #(% 1)) (reduce min))
            max-x (->> T (map #(% 0)) (reduce max))
            max-y (->> T (map #(% 1)) (reduce max))
            angle (* 2 Math/PI (rand))
            grad-fn (util/angle-gradient angle [min-x min-y] [max-x max-y])
            color (rand-nth pal)]
         (case variation
          :gradient-as-color
          (doseq [x (range min-x max-x) y (range min-y max-y)]
            (when (and (<= min-x x max-x) (<= min-y y max-y)
                       (util/poly-contains-p? T [x y])
                       (> (q/noise (* 1.37e1 x) (* 1.37e1 y)) (* 1 (grad-fn [x y]))))
              ;(q/set-pixel x y (q/color (grad-fn [x y])))))))))
              (q/set-pixel x y (apply q/color color))))
          :gradient-as-light
          (do
            (apply q/fill color
             (doseq [x (range min-x max-x) y (range min-y max-y)]
               (when (and (<= min-x x max-x) (<= min-y y max-y)
                          (util/poly-contains-p? T [x y])
                          (> (q/noise (* 1.37e1 x) (* 1.37e1 y)) (* 1 (grad-fn [x y]))))
                 ;(q/set-pixel x y (q/color (grad-fn [x y])))))))))
                 (q/set-pixel x y (q/color 0 0 100))))))
          :gradient-as-shade
          (do
            (apply q/fill color)
            (apply q/triangle (flatten T)) 
            (doseq [x (range min-x max-x) y (range min-y max-y)]
              (when (and (<= min-x x max-x) (<= min-y y max-y)
                         (util/poly-contains-p? T [x y])
                         (> (q/noise (* 1.37e1 x) (* 1.37e1 y)) (* 1 (grad-fn [x y]))))
                ;(q/set-pixel x y (q/color (grad-fn [x y])))))))))
                (q/set-pixel x y (q/color 0 100 0)))))
          :gradient-as-sat
          (doseq [x (range min-x max-x) y (range min-y max-y)]
            (when (and (<= min-x x max-x) (<= min-y y max-y)
                       (util/poly-contains-p? T [x y]))
                       ;(> (q/noise (* 1.37e1 x) (* 1.37e1 y)) (* 1 (grad-fn [x y]))))
              ;(q/set-pixel x y (q/color (grad-fn [x y])))))))))
              (q/set-pixel x y (q/color (color 0) (color 1) (* 100 (grad-fn [x y]))))))
          :no-gradient
          (do (apply q/stroke color) 
              (apply q/fill color) (apply q/triangle (flatten T))))))))
    
(comment 
  (->> [[400 0] [200 800] [600 800]]
       (map #(% 0))
       (reduce min)))

(declare sketch)
(defn sketch []
  (q/sketch
    :size [800 800]
    :setup setup
    ;:draw #(draw (* 2 Math/PI (rand)))
    ;:draw #(draw-split 14 1.8))
    :draw #(draw-rnd-grad :gradient-as-color 
                          (bit-shift-left 2 (rand-int 10))
                          (rand-nth (vals pals)))))

(comment
  (sketch)
  (quil.applet/with-applet
    drafts.gradients/sketch
    (q/save (str "out/gradients/gradients_" (subs (str (random-uuid)) 0 5) ".png")))) ;
