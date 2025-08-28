(ns shapes.polygons
  (:require [clojure.math :as math]))

(defn v-scale [scale v]
  (map #(* scale %) v))

#_(defn n-gon
    "Corners of a regular n-gon of diameter *scale*  centered at 0. 
  when drawing need to translate in drawing coordinates"
    ([n]
     (->> (range n)
          (mapcat #(vector (/ (math/cos (* 2 math/PI (/ % n))) 2)
                           (/ (math/sin (* 2 math/PI (/ % n))) 2)))))
    ([scale n] (mapv #(* scale %) (n-gon n))))

(defn n-gon
  "Corners of a regular n-gon of diameter *scale*  centered at [0 0]." 
  ([n]
   (->> (range n)
        (mapv #(vector (/ (math/cos (* 2 math/PI (/ % n))) 2)
                       (/ (math/sin (* 2 math/PI (/ % n))) 2)))))
  ([scale n] (mapv (partial v-scale scale) (n-gon n))))

(defn close-path 
  "Make a path loop to it's starting point"
  [path]
  (conj (vec path) (first path)))


#_(defn n-gon-sides 
    "Return the segments of a regular n-gon"
    ([n]
     (let [path (n-gon n)
           completed (take (* 5 n) (cycle path))]
       (partition 4 2 completed)))
    ([scale n]
     (let [path (n-gon scale n)
           completed (take (* 5 n) (cycle path))]
       (partition 4 2 completed))))

(defn rectangle
  [w h] 
  [(- (/ w 2)) (- (/ h 2)) 
   (/ w 2) (- (/ h 2))
   (/ w 2) (/ h 2)
   (- (/ w 2)) (/ h 2)])
   
(defn hourglass 
  [w h in]
  (let [[x0 xm1 xm2 x1] [(- (/ w 2)) (+ (- (/ w 2)) in) (- (/ w 2) in) (/ w 2)]
        [y0 ym y1] [(- (/ h 2)) 0 (/ h 2)]]
    (vector [x0 y0]
            [x1 y0]
            [xm2 ym]
            [x1 y1]
            [x0 y1]
            [xm1 ym])))
  
  
(comment 
  (hourglass 20 50 5)
  (rectangle 2.0 1.0)
  (math/to-degrees (math/atan2 -1 1))
  (n-gon 4) ;; (100 0 -50 87 -50 -87 100 0)
  (map #(+ -0.5 %) (n-gon 4))) 
