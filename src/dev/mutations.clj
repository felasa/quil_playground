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

(defn blob 
  [scale position mutations stds annealing]
  (let [ini-shape (close-path (n-gon scale 8))]
    (map #(map + % position) (mutate-path mutations annealing stds ini-shape))))

(comment 
  (mutate-segment :a [0 0] [1 1])
  (doseq [pt (n-gon 100 3)]
    (println pt)))

(defn masking-test [layers]
  (let [mask-layer (q/create-graphics (q/width) (q/height))
        shape-layer (q/create-graphics (q/width) (q/height))]
    (dotimes [i layers]
      (q/with-graphics mask-layer
        (q/clear)
        (q/background 255 0)
        (q/no-stroke)
        (q/fill 0 0 255 4)
        (dotimes [i 100]
          (q/ellipse (rand-int (q/width)) (rand-int (q/height)) 100 90)))
      (q/with-graphics shape-layer 
        (q/clear)
        (q/background 250 0)
        (q/no-stroke)
        (q/fill 128 12 12)
        (q/begin-shape)
        (doseq [;pt (mutate-path 4 0.8 [20 25 30 40 50 60 70 80] (close-path (n-gon 400 8)))
                pt (blob 400 [400 400] 4 [20 25 30 40 50 60 70 80] 0.7)]
          (apply q/vertex pt))
        (q/end-shape)
        (q/mask-image mask-layer))
      (q/blend shape-layer 0 0 800 800 0 0 800 800 :darkest))))

(comment 
  (declare testing-sketch)
  (q/defsketch testing-sketch
    :setup (fn [] (q/no-loop))
    :draw (partial masking-test 200)
    :draw-not (fn []
                (q/no-stroke)
                (q/fill 100 0 100 80)
                (dotimes [i 1]
                  (q/begin-shape)
                  (doseq [;pt (mutate-path 4 0.8 [20 25 30 40 50 60 70 80] (close-path (n-gon 400 8)))
                          pt (blob 400 [400 400] 4 [20 25 30 40 50 60 70 80] 0.7)]
                    (apply q/vertex pt))
                  (q/end-shape))
                (q/begin-shape)
                (q/fill 0 200 0 100)
                (q/translate 400 400)
                (doseq [pt (close-path (n-gon 400 8))]
                  (apply q/vertex pt))
                (q/end-shape))
    :size [800 800]
    :renderer :java2d))

(comment 
  (quil.applet/with-applet
    dev.mutations/testing-sketch))
