(ns util.fields
  (:require [quil.core :as q]
            [clojure.math :as math]))

(defn perlin-noise
  "Computes noise value at x,y optionally scaling the coords by scale"
  ([scale x y]
   (q/noise (* scale x) (* scale y)))
  ([x y] (perlin-noise 1 x y)))

(defn perlin-field
  "Returns a vector at x, y given by the angle produced by noise at x,y and scale"
  [scale]
  (fn [x y]
    (let [angle (* 2 math/PI (perlin-noise scale x y))
          dx (math/cos angle) dy (math/sin angle)]
      [dx dy])))

(defn curl-field
  "Return a vector at [x y] representing the curl field for perlin noise at that point"
  [scale]
  (fn [x y]
    (let [delta 1e-3
          dpx0 (q/noise (- (* scale x) delta) (* scale y))
          dpx1 (q/noise (+ (* scale x) delta) (* scale y))
          dpy0 (q/noise (* scale x) (- (* scale y) delta))
          dpy1 (q/noise (* scale x) (+ (* scale y) delta))]
      [(/ (- dpy1 dpy0) (* 2 delta))
       (- (/ (- dpx1 dpx0) (* 2 delta)))])))

(defn grid-field
  "'Snaps' field-fn to a resolution produced by grid-fn. 
    grid_fn is a function that transforms [x,y] -> [x', y'] meant to
    reduce the resolution but could be anythint"
  [grid-fn field-fn]
  (fn [x y] (field-fn (grid-fn x) (grid-fn y))))

(defn avg-fields [field-fn1 field-fn2]
  (fn [x y] (mapv #(/ % 2) (map + (field-fn1 x y) (field-fn2 x y)))))

(defn sum-fields [field-fn1 field-fn2]
  (fn [x y] (mapv + (field-fn1 x y) (field-fn2 x y))))

(defn circle-field [scale cx cy]
  (fn [x y]
    (let [v (mapv - [x y] [cx cy])
          d (q/dist cx cy x y)
          f (fn [p] (cond (< d 100) 0 :else (/ (* scale p) (* d d))))]
      (mapv f v))))


