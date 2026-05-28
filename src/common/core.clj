(ns common.core)

(defn hex-to-rgb
  [hex]
  (mapv read-string 
       (map #(reduce str "0x" %) 
            (partition 2 (subs hex 1)))))

(defn norm-squared 
  [p]
  (reduce + (map #(* % %) p)))

(defn norm [p]
  (Math/sqrt (norm-squared p)))

;; TODO: add arbitrary number of vectors
(defn v-add
  [p1 p2]
  (mapv + p1 p2))

(defn dot
  "dot product of two points"
  [p1 p2]
  (mapv * p1 p2))

(defn field-curve
  "Returns a sequence of points defined by the vector field field-fn
  segment lenght number of steps and initial point. field-fn must
  return a vector at a point"
  [field-fn segmen-len n-segments start-p]
  (loop [segments 1 [x y :as p] start-p return [[x y]]]
    (if (<= segments n-segments)
      (let [delta (field-fn x y)
            [x' y'] (->> (map #(* segmen-len %) delta)
                         (mapv + p))]
        (recur (inc segments)
               [x' y'] 
               (conj return [x' y'])))
      return)))

(defn scale-with-margin-fn
  "Returns a fn that scales point coordinates by scale but adds a padding for points that may 
  fall outside the drawing area"
  ([W H scale]
   (fn [[x y]] (vector (+ (* x scale) (/ W 4)) (+ (* y scale) (/ H 4))))))

(defn grid-transform
  "Reduces resolution of xy plane into fraction 'pixels'"
  ([width height fraction]
   (fn [[x y]] 
     (let [step-x (/ width fraction)
           step-y (/ height fraction)]
       (vector (* (quot x step-x) step-x) (* (quot y step-y) step-y)))))) 
