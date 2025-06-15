(ns shapes.polygons
  (:require [clojure.math :as math]))

(defn n-gon
  "Corners of a regular n-gon of diameter *scale*  centered at 0. 
  when drawing need to translate in drawing coordinates"
  ([n]
   (->> (range n)
        (mapcat #(vector (/ (math/cos (* 2 math/PI (/ % n))) 2)
                         (/ (math/sin (* 2 math/PI (/ % n))) 2)))))
  ([scale n] (mapv #(* scale %) (n-gon n))))

(defn n-gon-sides 
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
   
(comment 
  (rectangle 2.0 1.0)
  (math/to-degrees (math/atan2 -1 1))
  (n-gon 4) ;; (100 0 -50 87 -50 -87 100 0)
  (map #(+ -0.5 %) (n-gon 4)) 
  (n-gon-sides 3) 
  (mapv #(into [] %) (n-gon-sides 3))) 
