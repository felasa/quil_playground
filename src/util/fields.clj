(ns util.fields
  (:require [quil.core :as q]
            [clojure.math :as math]))

(defn pad-plane [factor]
  (fn [x y] (mapv + [(* factor 0.5) (* factor 0.5)] [(* x factor) (* y factor)])))


(defn perlin-noise
  "Computes noise value at x,y optionally scaling the coords by scale"
  ([scale]
   (fn [x y] (q/noise (* scale x) (* scale y))))
  ([] (perlin-noise 1)))

(defn perlin-noise3 
  [scale]
  (fn [x y z] (q/noise (* scale x) (* scale y) (* scale z))))

(defn normalize 
  "Make unit vector"
  [[x y]]
  (let [v [x y]
        N (q/mag x y)]
    (mapv #(/ % N) v)))

(defn perlin-field
  "Returns a fn that returns vector at x, y given by the angle produced by noise at x,y and scale"
  [scale]
  (fn [x y]
    (let [angle (* 2 math/PI ((perlin-noise scale) x y))
          dx (math/cos angle) dy (math/sin angle)]
      [dx dy])))

(defn perlin-field3
  [scale]
  (fn [x y z]
    (let [angle ((perlin-noise3 scale) x y z)
          dx (math/cos angle) dy (math/sin angle)]
      [dx dy])))
;; I dont undesrstand why making delta smaller makes it worse, since theoretically 
;;  it should be a better approximation to the derivative. df go to zero too quick.
;; withouth normalizing velocities are too small
(defn curl-field
  "Return a vector at [x y] representing the curl field for noise at that point.
   Normalized to unit vector"
  [scale]
  (fn [x y] 
    (let [delta 0.5
          dfy0 ((perlin-noise scale) x (- y delta))
          dfy1 ((perlin-noise scale) x (+ y delta))
          dfx0 ((perlin-noise scale) (- x delta) y)
          dfx1 ((perlin-noise scale) (+ x delta) y)
          [vx vy] [(/ (- dfy1 dfy0) (* 2 delta))
                   (/ (- dfx0 dfx1) (* 2 delta))]
          N (q/mag vx vy)]
      [vx vy])))
      ;(mapv #(/ % N) [vx vy]))))
      
(defn gen-curl 
  [potential-fn]
  (fn [x y] 
    (let [delta 10
          dfy0 (potential-fn x (- y delta))
          dfy1 (potential-fn x (+ y delta))
          dfx0 (potential-fn (- x delta) y)
          dfx1 (potential-fn (+ x delta) y)
          V [(/ (- dfy1 dfy0) (* 2 delta))
             (/ (- dfx0 dfx1) (* 2 delta))]]
          ;Ns (math/sqrt (reduce + (mapv #(* % %) V)))]
      (mapv #(* % 10) V))))

(defn modulate 
  [potential-fn-f potential-fn-g]
  (fn [x y] (* (potential-fn-f x y) (potential-fn-g x y))))

(defn grid-field
  "'Snaps' field-fn to a resolution produced by grid-fn. 
    grid_fn is a function that transforms [x,y] -> [x', y'] meant to
    reduce the resolution but could be anything"
  [grid-fn field-fn]
  (fn [x y] (field-fn (grid-fn x) (grid-fn y))))

#_(defn avg-fields [field-fn1 field-fn2]
    (fn [x y] (mapv #(/ % 2) (map + (field-fn1 x y) (field-fn2 x y)))))

(defn avg-fields
  [& field-fns]
  (fn [x y] 
    (let [nfields (count field-fns)]
      (->> field-fns 
           (map #(% x y))
           (apply map +)
           (mapv #(/ % nfields))))))

(defn add-fields
  [field-fn1 field-fn2]
  (fn [x y] (normalize (mapv + (field-fn1 x y) (field-fn2 x y)))))

(defn circle-field [scale cx cy]
  (fn [x y]
    (let [v (mapv - [x y] [cx cy])
          d (q/dist cx cy x y)
          f (fn [p] (cond (< d 100) 0 :else (/ (* scale p) (* d d))))]
      (mapv f v))))

(comment 
  (math/sqrt (* 450 450 2))
  (def papp (new processing.core.PApplet))
  (let [delta 0.1]
     (/ (- (. papp noise delta 0) (. papp noise 0 0)) 0.005)) 
  (. papp noise (rand-int 100)))
