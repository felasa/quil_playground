(ns util.core
  (:require [quil.core :as q]))

  
(defn gradient
  "Returns the fraction that x represents in the segment min-x to max-x.
   Sortof and inverse to lerp?"
  [min-x max-x x]
  (/ (- x min-x) max-x))
   
;; dx positive if -pi/2 < angle < pi/2
;;    negative if pi/2 < angle < 3pi/2 goes from max to min for x
;; dy positive if 0 < angle < pi
;;    negative if pi < angle < 2pi goes from max to min for y
;; 1.414 norm factor
;; 2.83

(defn angle-gradient
  "Gradient at an angle bounded by region"
  [angle [min-x min-y] [max-x max-y]]
  (fn [[x y]]
    (let [dx (Math/cos angle) dy (Math/sin angle)
          H (if (> (abs dx) (abs dy)) (/ (abs dx)) (/ (abs dy)))
          x' (cond (>= dx 0)
                   (/ (- x min-x) (- max-x min-x))
                   :else (/ (- x max-x) (- max-x min-x)))
          y' (cond (>= dy 0)
                   (/ (- y min-y) (- max-y min-y))
                   :else (/ (- y max-y) (- max-y min-y)))]
      (/ (+ (* x' dx) (* y' dy) ) H))))
      
(defn coord-from-idx
  "Map unidimensional index to two-dimensional coordinate of given width"
  [width idx]
  [(mod idx width)
   (quot idx width)])

(defn idx-from-coord
  "Map bi-dimensional coordinate from space of given width to one-dimensional index"
  [width [x y]]
  (+ x (* y width)))

(defn vector-add
  "Vector addition. Todo generalize to more elements"
  [& ps]
  (apply mapv + ps)) 

(defn vector-scale 
  "scale vector by factor l"
  [l p]
  (mapv (fn [x] (* l x)) p))

(defn vector-product
  "Vector coordinatewise product"
  [p1 p2]
  (mapv * p1 p2))

(defn vector-dot
  "Dot product of two vectors"
  [p1 p2]
  (reduce + (vector-product p1 p2)))

(defn point-lerp
  [p1 p2 t]
  (vector-add p1 (vector-scale t (mapv - p2 p1))))
  
(defn norm-squared 
  [p]
  (vector-dot p p))

(defn norm
  "L2 norm (euclidian lenght) of point p"
  [p]
  (Math/sqrt (norm-squared p)))

(defn dist 
  "Euclidian distance between p1 and p2"
  [p1 p2]
  (norm (mapv - p1 p2)))

;; How to handle norm zero?
(defn normalize 
  "Scale vector to unit lenght"
  [[x y :as p]]
  (let [N (norm p)]
    (if (> N 0) (vector-scale (/ N) p) [0 0])))

(defn poly-contains-p?
  "Checks y point p is inside polygon p. Borrowed from hobbe's genartlib"
  [poly p]
  (let [xs (mapv #(get % 0) poly)
        ys (mapv #(get % 1) poly)]
    (loop [i 0 j (dec (count poly)) ret false]
      (if (< i (count poly)) 
        (let [deltax (- (xs j) (xs i))
              yspread (- (p 1) (ys i))
              deltay (- (ys j) (ys i))]
          (if (and (not= (> (ys i) (p 1)) (> (ys j) (p 1)))
                   (< (p 0)
                      (+ (/ (* deltax yspread) deltay) (xs i))))
            (recur (inc i) i (not ret))
            (recur (inc i) i ret)))
        ret))))

(defn polys-overlap?
  "Checks if the polygons defined by collections of points poly1 and poly2 overlap"
  [poly1 poly2]
  (or
    (loop [rem poly2]
      (if-let [p (peek rem)]
        (if (poly-contains-p? poly1 p) true
          (recur (pop rem)))
        false))
    (loop [rem poly1]
      (if-let [p (peek rem)]
        (if (poly-contains-p? poly2 p) true
          (recur (pop rem)))
        false))))

(defn hex-to-rgb
  "Return the rgb (base 256) value of hex-string. TODO: make more robust?
   probably no need"
  [hex]
  (let [n (count hex)]
    (if (= n 7) ;;contains #
        (mapv read-string 
             (map #(reduce str "0x" %) 
                  (partition 2 (subs hex 1))))
        (mapv read-string 
             (map #(reduce str "0x" %) 
                  (partition 2 hex))))))

;just an alias
(def rgb-from-hex hex-to-rgb)

(defn scale-with-margin-fn
  "Returns a fn that scales point coordinates by scale and adds a padding for points that may 
  fall outside the drawing area"
  ([W H scale]
   (fn [[x y]] (vector (+ (* x scale) (/ W 4)) (+ (* y scale) (/ H 4))))))

(defn grid-transform-fn
  "Reduces resolution of xy plane into fraction 'pixels'"
  ([width height fraction]
   (fn [[x y]] 
     (let [step-x (/ width fraction)
           step-y (/ height fraction)]
       (vector (* (quot x step-x) step-x) (* (quot y step-y) step-y)))))) 

;; Drawing fns. maybe move things that depend on quil to other ns. becaus ethey cant be tested outside
;; of a pgraphics instance?
(defn draw-curve
  [pts] 
  (doseq [pair (partition 2 1 pts)]
    (apply q/line (flatten pair))))

(defn draw-shape
  [vxs]
  (q/begin-shape)
  (doseq [vx vxs]
    (apply q/vertex vx))
  (q/end-shape))

(defn w 
  "Returns width or width times p. taken from genartlib"
  ([] (q/width))
  ([p] (/ (w) p)))

(defn h 
  "Returns height or height times p. taken from genartlib"
  ([] (q/height))
  ([p] (/ (h) p)))
