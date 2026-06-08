(ns util.shapes
  (:require [util.core :refer [vector-scale]]
            [util.transform :refer [mutate-path]]))

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
        (mapv #(vector (/ (Math/cos (* 2 Math/PI (/ % n))) 2)
                       (/ (Math/sin (* 2 Math/PI (/ % n))) 2)))))
  ([scale n] (mapv (partial vector-scale scale) (n-gon n))))

(defn close-path 
  "Make a path loop to it's starting point"
  [path]
  (conj (vec path) (first path)))

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
  
(defn blob 
  "Mutates an n-gon mutation times. From tylehobbs tutorial on watercolor textures"
  [position scale stds annealing mutations]
  (let [ini-shape (close-path (n-gon scale 8))]
    (map #(map + % position) (mutate-path mutations annealing stds ini-shape))))
