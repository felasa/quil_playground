(ns dev.mutations
  (:require [quil.core :as q]
            [shapes.polygons :as poly]
            [clojure.math :as math]))

(defn scale-v [scale v]
  (map #(* scale %) v))

(defn mutate-segment 
  [std p1 p2]
  (let [[x1 y1] p1 [x2 y2] p2
        [xm ym] (map #(/ % 2.0) (mapv + p1 p2))]
    (vector 
      p1 
      (list (+ xm (* std (q/random-gaussian)))
            (+ ym (* std (q/random-gaussian))))
      p2)))

(defn mutate-path
  ([stds pts]
   (loop [stds (cycle stds) 
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

(defn n-gon
  "Corners of a regular n-gon of diameter *scale*  centered at [0 0]." 
  ([n]
   (->> (range n)
        (mapv #(vector (/ (math/cos (* 2 math/PI (/ % n))) 2)
                       (/ (math/sin (* 2 math/PI (/ % n))) 2)))))
  ([scale n] (mapv (partial scale-v scale) (n-gon n))))

(defn close-path 
  "Make a path loop to it's starting point"
  [path]
  (conj (vec path) (first path)))

(comment 
  (mutate-segment :a [0 0] [1 1])
  (doseq [pt (n-gon 100 3)]
    (println pt)))
(comment 
  (declare testing-sketch)
  (q/defsketch testing-sketch
    :setup (fn [] (q/no-loop))
    :draw (fn []
            (q/no-stroke)
            (q/translate 400 400)
            (q/fill 100 0 100 8)
            (dotimes [i 40]
              (q/begin-shape)
              (doseq [pt (mutate-path 4 0.8 [20 25 30 40 50 60 70 80] (close-path (n-gon 400 8)))]
                (apply q/vertex pt))
              (q/end-shape))
            (q/begin-shape)
            (q/fill 0 200 0 0)
            (doseq [pt (close-path (n-gon 400 8))]
              (apply q/vertex pt))
            (q/end-shape))
    :bg-color 50
    :size [800 800]
    :renderer :java2d))

(comment 
  (quil.applet/with-applet
    dev.mutations/testing-sketch
    ;(mutate-segment 4 [0 0] [1 1])
    (mutate-path 2  [0.01 0.02 0.03] [[0 0] [1 1] [2 2] [3 3] [0 0]]) ;; [0 0]
    ;(mutate-path [50 60 100] (close-path (n-gon 400 3)))
;; [[0 0] [0.4973365849256515 0.5040589639544487] [1 1] [1.5298498034477235 1.5326583456993104]
;;  [2 2] [2.512829790711403 2.4952260667085646] [3 3] [1.4833810102939606 1.5063845378160476]
;;  [0 0]]
    #_(do (q/begin-shape)
          (doseq [ptx (partition 2 (poly/n-gon 3))]
            (apply q/vertex ptx))
          (q/end-shape))))
;; [[0 0]
;;  (0.5047583442926407 0.5090721398591995)
;;  [1 1]
;;  (1.522718858718872 1.4930974918603896)
;;  [2 2]
;;  (2.4479117155075074 2.434350094795227)
;;  [3 3]
;;  (1.487816082239151 1.5050051808357239)
;;  [0 0]]
