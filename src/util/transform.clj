(ns util.transform
  (:require [quil.core :as q]
            [util.random :as random :refer [draw-binormal]]
            [util.core :as uc]))

;; TODO: could use a revision
(defn rotate-polygon
  "Rotates a polygon clockwise about its centroid.  The theta argument determines
   how the polygon is rotated.  `points` is a sequence of [x y] pairs that
   define the polygon"
  ([centroid angle points] 
   (if (zero? angle)
     points
     (let [xs (mapv #(get % 0) points) ys (mapv #(get % 1) points)
           [x-centroid y-centroid] centroid 
           points (map vector xs ys)]
       (map (fn [[x y]]
              (let [current-angle (angle x-centroid y-centroid x y)
                    new-angle (+ current-angle angle)
                    hypot (uc/dist [x y] [x-centroid y-centroid])
                    x-offset (* hypot (Math/cos new-angle))
                    y-offset (* hypot (Math/sin new-angle))]
                [(+ x-offset x-centroid) (+ y-offset y-centroid)])
              points)))))
  ([angle points]
   (if (zero? angle)
     points
     (let [xs (map first points)
           ys (map second points)
           min-x (apply min xs)
           max-x (apply max xs)
           min-y (apply min ys)
           max-y (apply max ys)
           x-centroid (/ (+ min-x max-x) 2.0)
           y-centroid (/ (+ min-y max-y) 2.0)
           points (map vector xs ys)]
       (rotate-polygon [x-centroid y-centroid] angle points)))))

(defn transform-points
  "Scales a set of points relative to center"
  [scale center pts]
  (->> pts 
       (map (fn [v] (map - center v)))
       (map (fn [v] (map #(* scale %) v)))
       (map (fn [v] (map + center v)))))

(defn mutate-segment 
  ([std p1 p2]
   (let [[xm ym] (map #(/ % 2.0) (mapv + p1 p2))]
     (vector 
       p1 
       (vector (+ xm (* std (q/random-gaussian)))
               (+ ym (* std (q/random-gaussian))))
       p2)))
  ([p1 p2] (mutate-segment 1 p1 p2)))

(defn mutate-segment-with-fn
  ([mutate-fn factor p1 p2]
   (let [[xm ym] (map #(/ % 2.0) (mapv + p1 p2))]
     (vector 
       p1 
       (mapv + [xm ym] (map #(* factor %) (mutate-fn xm ym)))
       p2)))
  ([mutate-fn p1 p2] (mutate-segment-with-fn mutate-fn 1 p1 p2)))
  
(defn mutate-path
  ([stds pts]
   (loop [stds (if (seqable? stds) (cycle stds) (cycle [stds])) 
          segments (partition 2 1 pts)
          return []]
     (if-let [segment (first segments)]
       (let [mutated (apply mutate-segment (first stds) segment)]
         (recur (rest stds) (rest segments) (into return (take 2 mutated))))
       (conj return (last pts)))))
  ([times stds pts]
   (loop [i 1 stds (cycle stds) pts pts] 
     (if (<= i times)
       (recur (inc i) (mapcat (partial repeat 2) stds) (mutate-path stds pts))
       pts)))
  ([times annealing stds pts]
   (loop [i 1 stds (cycle stds) pts pts] 
     (if (<= i times)
       (recur (inc i) (map #(* % annealing) (mapcat (partial repeat 2) stds)) (mutate-path stds pts))
       pts))))

(defn mutate-path-with-fn
  ([mutate-fn factors pts]
   (loop [stds (if (seqable? factors) (cycle factors) (cycle [factors])) 
          segments (partition 2 1 pts)
          return []]
     (if-let [segment (first segments)]
       (let [mutated (apply mutate-segment-with-fn mutate-fn (first stds) segment)]
         (recur (rest stds) (rest segments) (into return (take 2 mutated))))
       (conj return (last pts)))))
  ([mutate-fn times factors pts]
   (loop [i 1 factor (cycle factors) pts pts] 
     (if (<= i times)
       (recur (inc i) (mapcat (partial repeat 2) factor) (mutate-path-with-fn mutate-fn factor pts))
       pts)))
  ([mutate-fn times annealing factors pts]
   (loop [i 1 stds (cycle factors) pts pts] 
     (if (<= i times)
       (recur (inc i)
              (map #(* % annealing) (mapcat (partial repeat 2) stds))
              (mutate-path-with-fn mutate-fn stds pts))
       pts))))

;old version
#_(defn mutate-segment
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


(defn mutate-segment-gauss
  "Older version kept for compat"
  ([std x1 y1 x2 y2]
   (let [[xm ym :as mp] [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)]
         [xn yn :as Z] (draw-binormal [xm ym] [std std])]
     [x1 y1 xn yn x2 y2])))

#_(defn mutate-segment-gauss2
    ([std x1 y1 x2 y2]
     (let [[xm ym :as mp] [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)]
           [xn yn :as Z] (vector (random/gauss xm std) (random/gauss ym std))]
       [x1 y1 xn yn x2 y2])))

#_(defn mutate-path
    ([std x1 y1 x2 y2] 
     (mutate-segment-gauss std x1 y1 x2 y2))
    ([std x1 y1 x2 y2 & xys] 
     (let [[x3 y3] (take 2 xys)
            xyss (drop 2 xys)]
       (into (subvec (mutate-path std x1 y1 x2 y2) 0 4)
             (apply mutate-path std x2 y2 x3 y3 xyss)))))

#_(defn mutate-path2
    ([std x1 y1 x2 y2] 
     (mutate-segment-gauss2 std x1 y1 x2 y2))
    ([std x1 y1 x2 y2 & xys] 
     (let [[x3 y3] (take 2 xys)
            xyss (drop 2 xys)]
       (into (subvec (mutate-path2 std x1 y1 x2 y2) 0 4)
             (apply mutate-path2 std x2 y2 x3 y3 xyss)))))

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
      
#_(defn mutate-path-v2
    [std xys]
    (loop [remaining xys
           counter (count xys)
           return []]
      (if (< counter 4) (into return (drop (- (count xys) 2) xys))
        (let [[x1 y1 x2 y2] (take 4 remaining)]
          (recur (subvec remaining 2)
                 (dec (dec counter))
                 (into return (take 4 (mutate-segment-gauss2 std x1 y1 x2 y2))))))))

#_(defn mutate-path-variable 
    [stds pts]
    (loop [segments (partition 2 1 pts)
           segment-stds (cycle (if (seqable? stds) stds (list stds)))
           return []]
      (if-let [current-segment (first segments)]
        (let [[p1 p2] current-segment
              current-std (first stds)
              e (vector (random/gauss 0 current-std) (random/gauss 0 current-std))
              pm (map #(mapv + e %) (mapv #(/ % 2) (map + p1 p2)))]
          (recur (rest segments) (rest segment-stds) (into return [p1 pm p2])))
        return)))

;; TODO: rename
(defn transform-shape
  "Applies a transformation (traslation, rotation, scale) before applying draw-fun. controled by 
   params map with keys :dx and :dy for traslation, :scale for scaling, :angle (in radians) for 
   rotations"
  [draw-fn params]
  (let [{:keys [dx dy angle scale]} params]
    (q/push-matrix)
    (when scale (q/scale  scale))  
    (when dx    (q/translate dx 0))
    (when dy    (q/translate 0 dy))
    (when angle (q/rotate angle))
    (draw-fn)
    (q/pop-matrix)))

