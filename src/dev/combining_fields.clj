(ns dev.combining-fields
  (:require [quil.core :as q]
            [util.random :as rnd]
            [util.core :refer :all]
            [clojure.test :refer [deftest is]]) 
  (:import fastnoise.FastNoiseLite))

(def fnl (FastNoiseLite. 123))
(.SetFrequency fnl 0.1)
(comment (.GetNoise fnl 50 2000))
(defn avg-fields
  [f1 f2]
  (fn [p]
    (let [v1 (f1 p) v2 (f2 p)
          m (mapv + v1 v2)
          N (norm m)]
      (mapv #(/ % N) m))))

(defn poly-contains-p? [poly p]
  (let [xs (mapv #(get % 0) poly)
        ys (mapv #(get % 1) poly)]
    (loop [i 0 j (dec (count poly)) ret false]
           
      (if (< i (count poly)) 
        (let [deltax (- (xs j) (xs i))
              yspread (- (p 1) (ys i))
              deltay (- (ys j) (ys i))]
          (if (and (not= (> (ys i) (p 1)) (> (ys j) (p 1)))
                   (< (p 0) (+ (/ (* deltax yspread) deltay) (xs i))))
            (recur (inc i) i (not ret))
            (recur (inc i) i ret)))
        ret))))
(comment 
  (poly-contains-p? [[-1.0 1.0] [1.0 1.0] [1.0 -1.0] [-1.0 -1.0]]
                    [-1.000 -1.0]))                 

(defn polys-intersect? [poly1 poly2]
  (if (or (nil? poly1) (nil? poly2)) false
    (or (not (not-any? (partial poly-contains-p? poly1) poly2))
        (not (not-any? (partial poly-contains-p? poly2) poly1)))))
(comment 
  (polys-intersect? [[0 0] [1 0] [1 1] [1 1]] 
                    [[0 0] [0.5 0] [0 0.5] [0.5 0.5]])
  (polys-intersect? [[0 0] [1 0] [1 1] [0.5 0.5]] 
                    [[-1 0] [-1 -1] [-0.5 -0.5] [-0.5 0.0]]))

(defn pull-field 
  ([scale center]
   (fn [p] 
     (let [dir (mapv - center p)
           norm (norm dir)
           delta (if (> norm 0) (mapv #(/ % (* norm norm) (/ scale)) dir) [0 0])]
        delta)))
  ([center]
   (pull-field 1 center)))

(comment 
  ((pull-field [400 400]) [399 399]) ;; [0.4999999999999999 0.4999999999999999]
  ((pull-field [400 400]) [0 0]) ;; [0.00125 0.00125]
  ((pull-field 100 [400 400]) [0 0])) ;; [0.125 0.125]

(defn perlin-field
  [scale]
  (fn [p]
    (let [scaled (mapv #(* scale %) p)
          n (apply q/noise scaled)
          [dx dy :as delta] [(Math/cos (* 2 n Math/PI)) (Math/sin (* 2 n Math/PI))]]
      delta)))
         
(defn setup []
  (q/no-loop))

(defn draw-field-curve
  [field-fn n-segments segmen-len p-ini]
  (loop [i n-segments
         start p-ini]
    (when (> i 0)
      (let [delta (mapv #(* segmen-len %) (field-fn start))
            [nx ny :as end] (mapv + start delta)]
        (q/line (start 0) (start 1) nx ny)
        (recur (dec i) end)))))
            
(defn viz-field [field-fn]
  ;(doseq [x (range 0 (q/width) 50) y (range 0 (q/height) 50)]
  (dotimes [_ 500]
   (let [x (rand-int  (q/width)) y (rand-int (q/height))
         delta (field-fn [x y])
         [nx ny] (mapv + [x y] delta)]
     (q/stroke 0 50)
     (q/stroke-weight 1)
     (q/line x y nx ny)
     (q/stroke 0)
     (q/stroke-weight 2)
     (q/point nx ny))))

(defn flow-curve
  "Returns the coords that make a path with a flow field. No drawing is done"
  [field-fn n-steps step-len p-ini]
  (loop [i 1
         p p-ini
         ret [p-ini]]
    (if (<= i n-steps)
      (let [V  (mapv #(* step-len %) (field-fn p))
            p' (mapv + p V)]
        (recur (inc i) p' (conj ret p')))
      ret)))

(comment
  (flow-curve (perlin-field 0.003)
          100 5 [100 100]))

(defn dband-vs
  [field-fn n-steps step-len p-ini p-ini']
  (let [side1 (flow-curve field-fn n-steps step-len p-ini)
        side2 (flow-curve field-fn n-steps step-len p-ini')]
    (concat side1 (reverse side2))))

(defn dband
  [field-fn n-steps step-len p-ini p-ini']
  (let [side1 (flow-curve field-fn n-steps step-len p-ini)
        side2 (flow-curve field-fn n-steps step-len p-ini')]
    ;(into side1 (reverse side2))
    (q/begin-shape)
    (doseq [vx (conj (into side1 (reverse side2)) (first side1))]
      (apply q/vertex vx))
    (q/end-shape)))

(defn some?
   [pred coll]
   (not (every? (complement pred) coll)))

(defn non-overlapping
  []
  (q/background 0)
  (q/no-stroke)
  ;(q/stroke 255 0 100)
  (q/fill 255 0 255 128)
  (loop [i 1 polys []]
    (let [x (rand-int (q/width)) y (rand-int (q/height))
          x' (+ x (* 40 (q/random-gaussian)))
          y' (+ y (* 40 (q/random-gaussian)))
          band
          (dband-vs (avg-fields (perlin-field 0.003) (pull-field 500 [400 400]))
            35 6 [x y] [x' y'])]
      (when 
        (and (<= i 5)
             (every? #((complement polys-intersect?) band %) polys))
        (when (= (mod i 5) 0) (println i))
        (q/begin-shape)
        (doseq [vs band] (apply q/vertex vs))
        (q/end-shape)
        (recur (inc i)
               (conj polys band))))))
        
(defn non-overlapping-tst
  []
  (loop [i 1 polys []]
    (let [x (rand-int 800) y (rand-int 800)
          x' (+ x (rand-int 20)) y' (+ y (rand-int 20))
          band
          (dband-vs (avg-fields (perlin-field 0.003) (pull-field 500 [400 400]))
            35 6 [x y] [x' y'])]
      (if 
        (and (<= i 500)
             (every? #((complement polys-intersect?) band %) polys))
        (recur (inc i)
               (conj polys band))
        polys))))

(comment
  (q/defsketch sketch 
    :size [800 800]
    :setup setup
    :settings #(q/smooth 16)
    :drawa #(viz-field (fn [p] (mapv (fn [v] (* 40 v)) ((perlin-field 0.05) p))))
    :drawa #(viz-field (pull-field 6000 [400 400]))
    :drawa
    (fn []
      (q/background 10)
      (q/noise-seed (rand-int 1000))
      (q/noise-detail 8)
      (q/stroke 255 0 255 1)
      ;(doseq [x (range 0 (q/width) 25) y (range 0 (q/height) 25)]
      (dotimes [_ 500000]   
        (let [[x y] [(rand-int (q/width)) (rand-int (q/height))]]
          ;(draw-field-curve (pull-field [500 500]) 1 1000 [x y])
          (draw-field-curve 
            (avg-fields (perlin-field 0.003)
                        (pull-field 500 [400 400]))
            10 5 [x y]))))
    :draw
    (fn []
      (q/background 00)
      ;; thought: order which bands are drawn first while keeping positions random
      (let [g1 (q/create-graphics 760 760)
            thresh (rand)]
        (q/with-graphics g1 
          (q/color-mode :hsb 359 100 100 1.0)
          (q/background 219 14 8)
          (q/no-stroke)
           ;(q/stroke 255 0 100)
          (dotimes [_ 120]
            (if (> (rand) thresh) (q/fill 297 36 65 0.7)
              (q/fill 218 79 53 0.7))
            (let [x (rand-int (q/width)) y (rand-int (q/height))
                  x' (+ x (* 50 (q/random-gaussian))) y' (+ y (* 50 (q/random-gaussian)))
                  band
                  (dband-vs (avg-fields (perlin-field 0.003)
                                (pull-field 500 [400 400]))
                            70 3 [x y] [x' y'])]
              (q/begin-shape)
              (doseq [vs band]
                (apply q/vertex vs))
              (q/end-shape)
              #_(dband
                    (avg-fields (perlin-field 0.003)
                                (pull-field 500 [400 400]))
                    70 3 [x y] [x' y'])))
          (doseq [x (range (q/width)) y (range (q/height))]
            (let [scale 1.35153
                  xs (* scale x) ys (* scale y) 
                  n  (q/noise xs ys)]
                  ;n2 (.GetNoise fnl xs ys)] 
              (when (and (> n 0.75) (= (q/color 219 14 8) (q/get-pixel x y))) 
                    (q/set-pixel x y (q/color 34 51 99)))
              #_(when (and (> n2 0.6)
                          (and (not= (q/color 0)  (q/get-pixel x y))
                               (not= (q/color 255)  (q/get-pixel x y))))
                     (q/set-pixel x y (q/color 240 0 240))))))
        (q/image g1 20 20)
        #_(do 
            (.SetFrequency fnl 1.3)
            (let [ 
                  maxR (norm [400 400])
                  gradient (fn [r] (/ (- maxR r) maxR))]
              (doseq [x (range (q/width)) 
                      y (range (q/height))]
                (let [R (norm (mapv - [x y] [(/ (q/width) 2) (/ (q/height) 2)]))]
                  (when (> (- (/ (+ 1 (.GetNoise fnl x y)) 2.0) 0.3) (gradient R))
                    (q/set-pixel x y (q/color 0)))))))))))
              
  ;; try to remove overlapping bands  
    ;:drawa
    ;non-overlapping)) 
  
(comment 
  (Math/atan2 -0 400)
  (quil.applet/with-applet dev.combining-fields/sketch
    (non-overlappinrg-tst)))

