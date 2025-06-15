(ns util.transform
  (:require [quil.core :as q]
            [clojure.math :as math]
            [shapes.polygons :as poly]))

(defn mutate-segment
  [x1 y1 x2 y2]
  (let [v (mapv - [x2 y2] [x1 y1])
        lenv (math/sqrt (reduce + (map #(* % %) (mapv - [x1 y1] [x2 y2]))))
        pm (mapv #(/ % 2) (mapv + [x1 y1] [x2 y2]))
        segment-angle (+ (math/to-radians -90) (math/atan2 (- (v 1)) (v 0)))
        direction (math/to-radians 170);(- (rand 180))
        theta-p (+ segment-angle direction)
        magnitude (* 0.2 lenv) ;(rand (/ lenv 30))
        mx (* (math/cos theta-p)  magnitude)
        my (* (math/sin theta-p)  magnitude)
        nx (+ (pm 0) mx) ny (+ (pm 1) my)]
    [x1 y1 nx ny x2 y2])) 

(defn draw-binormal 
  ([]
   (let [u1 (rand)
         u2 (rand)]
     [(* (math/sqrt (* -2 (math/log u1))) (math/cos (* 2 math/PI u2)))
      (* (math/sqrt (* -2 (math/log u1))) (math/sin (* 2 math/PI u2)))]))
  ([[xm ym :as mean]] 
   (let [Z (draw-binormal)]
     (mapv + mean Z)))
  ([mean [stdx stdy :as std]]
   (let [Z (draw-binormal)]
     (mapv + mean (map * std Z)))))

(comment 
  (draw-binormal [300 300] [1 1]))

(defn rbinorm
  [times & params]
  (let [sample (transient (vector))]
    (dotimes [n times]
      (conj! sample (apply draw-binormal params)))
    (persistent! sample)))

(defn rnorm
  ([times]
   (let [sample (transient (vector))]
     (dotimes [n times]
       (conj! sample ((draw-binormal) 0)))
     (persistent! sample)))
  ([times mean std] (mapv #(+ mean %) (map #(* std %) (rnorm times)))))

(defn mutate-segment-gauss
  ([std x1 y1 x2 y2]
   (let [[xm ym :as mp] [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)]
         [xn yn :as Z] (draw-binormal [xm ym] [std std])]
     [x1 y1 xn yn x2 y2])))

(defn mutate-path
  ([std x1 y1 x2 y2] 
   (mutate-segment-gauss std x1 y1 x2 y2))
  ([std x1 y1 x2 y2 & xys] 
   (let [[x3 y3] (take 2 xys)
          xyss (drop 2 xys)]
     (into (subvec (mutate-path std x1 y1 x2 y2) 0 4)
           (apply mutate-path std x2 y2 x3 y3 xyss)))))

(defn mutate-path-v
  [std v]
  (loop [remaining v
         counter (count v)
         return []]
    (if (< counter 4) (into return (drop (- (count v) 2) v))
      (let [[x1 y1 x2 y2] (take 4 remaining)]
        (recur (subvec remaining 2)
               (dec (dec counter))
               (into return (take 4 (mutate-segment-gauss std x1 y1 x2 y2))))))))
      

(comment 
  (let [annealing-rate 0.9
        std 10
        n-gon (poly/n-gon 100 5)
        n-gon-cycled (into n-gon (take 2 n-gon))
        mutated (apply mutate-path std n-gon-cycled)
        mutated2 (apply mutate-path (* std annealing-rate) mutated)]
    mutated2)
  (mutate-path 0.05 0 0 1 1 2 2 3 3) 
  (flatten (map #(apply mutate-segment-gauss 10 %)
                (partition 4 2 (flatten (mutate-segment-gauss 1 200 200 400 400))))))

(defn transform-shape
  "Applies a transformation (traslation, rotation, scale) before applying draw-fun. controled by 
   params map with keys :dx and :dy for traslation, :scale for scaling, :angle (in radians) for 
   rotations"
  [draw-fn params]
  (let [{dx :dx dy :dy angle :angle scale :scale} params]
    (q/push-matrix)
    (when dx
      (q/translate  dx 0))
    (when dy
      (q/translate 0 dy))
    (when angle
      (q/rotate  angle))
    (when scale
      (q/scale  scale))  
    (draw-fn)
    (q/pop-matrix)))

