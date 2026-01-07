(ns util.fieldlib
  (:require [quil.core :as q]))

(defn scale-with-margin-fn
  "Returns a fn that scales point coordinates by scale but adds a padding for points that may 
  fall outside the drawing area"
  ([scale]
   (fn [[x y]] (vector (+ (* x scale) (/ (q/width) 4)) (+ (* y scale) (/ (q/height) 4)))))
  ([W H scale]
   (fn [[x y]] (vector (+ (* x scale) (/ W 4)) (+ (* y scale) (/ H 4))))))
(defn grid-transform
  "Reduces resolution of xy plane into fraction 'pixels'"
  ([fraction]
   (fn [[x y]] 
     (let [step-x (/ (q/width) fraction)
           step-y (/ (q/height) fraction)]
       (vector (* (quot x step-x) step-x) (* (quot y step-y) step-y))))) 
  ([W H fraction]
   (fn [[x y]] 
     (let [step-x (/ W fraction)
           step-y (/ H fraction)]
       (vector (* (quot x step-x) step-x) (* (quot y step-y) step-y)))))) 

(defn perlin-noise
  "Computes noise value at x,y optionally scaling the coords by scale"
  ([scale]
   (fn [[x y]] (apply q/noise ((scale-with-margin-fn scale) [x y])))) 
  ([] (perlin-noise 1)))

(defn perlin-field
  "Returns a vector at x, y given by the angle produced by noise at x,y and scale"
  [scale]
  (fn [[x y]]
    (let [angle (* 2 q/PI ((perlin-noise scale) [x y]))
          dx (q/cos angle) dy (q/sin angle)]
      [dx dy])))

(comment
  (doseq [x (range -100 801 100) y (range -100 801 100)] 
    (println ((scale-with-margin-fn 800 800 0.5) [x y])))
  (doseq [x (range 0 801 50) y (range 0 801 50)]
    (println ((grid-transform 800 800 10) [x y])))
  (declare sketch)
  (defn draw-fn [[x-ini y-ini]]
    (loop [iter 1 [x y :as p] [x-ini y-ini]]
      (when (< iter 50) 
        (let [[dx dy :as dp] ((perlin-field 0.0005) ((grid-transform 5) p))
              [x' y'] (mapv + p (mapv * [5 5] dp))]
          (q/line x y x' y')
          (recur (inc iter) [x' y']))))) 
  (q/defsketch sketch 
    :size [800 800]
    :setup (fn [] (q/no-loop) (q/background 255))
    :draw #(doseq [x (range 0 801 10)
                   y (range 0 801 10)]
             (draw-fn [x y]))))
