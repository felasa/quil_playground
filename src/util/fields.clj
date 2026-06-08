(ns util.fields
  (:require [quil.core :as q]
            [util.core :refer [scale-with-margin-fn h w normalize vector-add vector-scale]]
            [clojure.math :as math]))

;unknown
;(defn pad-plane [factor]
;  (fn [x y] (mapv + [(* factor 0.5) (* factor 0.5)] [(* x factor) (* y factor)])))

(defn perlin-noise
  "Computes noise value at x,y optionally scaling the coords by scale"
  ([scale]
   (fn [[x y]] (apply q/noise ((scale-with-margin-fn (w) (h) scale) [x y])))) 
  ([] (perlin-noise 1)))

(defn perlin-field
  "Returns a vector at x, y given by the angle produced by noise at x,y and scale"
  [scale]
  (fn [[x y]]
    (let [angle (* q/TWO-PI ((perlin-noise scale) [x y]))
          dx (q/cos angle) dy (q/sin angle)]
      [dx dy])))

;used in animated skecth
(defn perlin-noise3 
  "3d perlin"
  [scale]
  (fn [x y z] (q/noise (* scale x) (* scale y) (* scale z))))

(defn perlin-field3
  [scale]
  (fn [x y z]
    (let [angle (* q/TWO-PI ((perlin-noise3 scale) x y z))
          dx (math/cos angle) dy (math/sin angle)]
      [dx dy])))

;; Making delta smaller makes it worse, since theoretically 
;;  it should be a better approximation to the derivative. df go to zero too quick.
;; withouth normalizing velocities are too small
(defn curl-field
  "Return a vector at [x y] representing the curl field for noise at that point.
   Normalized to unit vector"
  [scale]
  (fn [x y] 
    (let [delta 0.5
          dfy0 ((perlin-noise scale) [x (- y delta)])
          dfy1 ((perlin-noise scale) [x (+ y delta)])
          dfx0 ((perlin-noise scale) [(- x delta) y])
          dfx1 ((perlin-noise scale) [(+ x delta) y])
          [vx vy] [(/ (- dfy1 dfy0) (* 2 delta))
                   (/ (- dfx0 dfx1) (* 2 delta))]
          N (q/mag vx vy)]
      [vx vy])))
      ;(mapv #(/ % N) [vx vy]))))
      
(defn gen-curl 
  "Returns curl-field fn for given potential-fn"
  ([delta potential-fn]
   (fn [x y] 
     (let [;delta 10
           dfy0 (potential-fn x (- y delta))
           dfy1 (potential-fn x (+ y delta))
           dfx0 (potential-fn (- x delta) y)
           dfx1 (potential-fn (+ x delta) y)
           V [(/ (- dfy1 dfy0) (* 2 delta))
              (/ (- dfx0 dfx1) (* 2 delta))]]
           ;Ns (math/sqrt (reduce + (mapv #(* % %) V)))]
       (mapv #(* % 10) V)))))

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
  ([& field-fns]
   (fn [[x y]] 
     (let [n-fields (count field-fns)]
       (->> field-fns 
            (map #(% [x y]))
            (apply map +)
            (mapv #(/ % n-fields)))))))

(defn add-fields
  [& field-fns]
  (fn [p] 
    (->> field-fns
         (map #(% p))
         (apply vector-add))))

(defn normalize-field
  "Transform field to output unit vector in same direction"
  [field-fn]
  (comp normalize field-fn))

;; TODO: Implement correctly
(defn circle-field
  "Return vector tangent to circle with radius at [x y], centered at middle, normalized"
  [W H [x y]]
  (let [[opx opy] (mapv - [x y] [(/ W 2) (/ H 2)])]
     (normalize [(- opy) opx])))

(comment 
  (q/sketch 
    :size [800 800]
    :setup (fn [] (q/no-loop))
    :draw 
    (fn []
      (doseq [x (range 0 800 50)
              y (range 0 800 50)]
        (let [v (mapv #(* 1 %) ((concentric-field 1 [300 400]) [x y]))
              p' (mapv + [x y] v)]
          (q/fill 255 0 0)
          (q/ellipse x y 10 10)
          (q/fill 0 255 0)
          (q/ellipse (p' 0) (p' 1) 10 10)
          (q/line [x y] p')))))) 

(defn concentric-field
  "Vector field fn that points to c with strenght proportional to distance * scale" 
  ([scale c]
   (fn [p]
      (let [v (mapv - c p)] 
         (vector-scale scale v))))
  ([c] (concentric-field 1 c)))

(defn field-curve
  "Returns a sequence of points defined by the vector field field-fn
  segment lenght, number of steps and initial point. field-fn must be a
  fn that return a vector at a point ([x0 x1] -> [v0 v1])"
  [field-fn segmen-len n-segments start-p]
  (loop [segments 1
         [x y :as p] start-p
         return [[x y]]]
    (if (<= segments n-segments)
      (let [delta (field-fn [x y])
            [x' y'] (->> (map #(* segmen-len %) delta)
                         (mapv + p))]
        (recur (inc segments)
               [x' y'] 
               (conj return [x' y'])))
      return)))

(comment
  (field-curve (fn [[x y]] [(+ x 1) (+ y 2)])
               10 3 [0 0]))
