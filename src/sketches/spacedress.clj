(ns sketches.spacedress
  (:require [quil.core :as q] 
            [util.core :refer [norm polys-overlap?]]
            [util.fields :as fields])) ;[perlin-field avg-fields normalize-field]]))

(defn middle-field
  "Sums and normalizes two fields"
  [f1 f2]
  (fields/normalize-field (fields/add-fields f1 f2)))
  
;; Not the same as fields/concentric-field because it's weighted down by the square of the distance
;; to the center
(defn pull-field 
  "Like concentric but weird"
  ([scale center]
   (fn [p] 
     (let [dir (mapv - center p)
           norm (norm dir)
           delta (if (> norm 0) (mapv #(/ % (* norm norm) (/ scale)) dir) [0 0])]
       delta)))
  ([center] (pull-field 1 center)))

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
  (fields/field-curve (fields/perlin-field 0.003)
          5 100 [100 100]))

(defn dband-vs
  "Returns a 'band' that moveds along a flow field"
  [field-fn n-steps step-len p-ini p-ini']
  (let [side1 (fields/field-curve field-fn step-len n-steps p-ini)
        side2 (fields/field-curve field-fn step-len n-steps p-ini')]
    (concat side1 (reverse side2))))

;; TODO: FIX
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
          (dband-vs (middle-field (fields/perlin-field 0.003) (pull-field 500 [400 400]))
            35 6 [x y] [x' y'])]
      (when 
        (and (<= i 5)
             (every? #((complement polys-overlap?) band %) polys))
        (when (= (mod i 5) 0) (println i))
        (q/begin-shape)
        (doseq [vs band] (apply q/vertex vs))
        (q/end-shape)
        (recur (inc i)
               (conj polys band))))))
        
(defn drawing
  [bands border?]
  (q/background 00)
  ;; thought: order which bands are drawn first while keeping positions random
  (let [g1 (q/create-graphics 760 760)
        thresh (rand)]
    (q/with-graphics g1 
      (q/color-mode :hsb 359 100 100 1.0)
      (q/background 219 14 8)
      (q/no-stroke)
      (when border? (q/stroke 255 0 0))
      ; 120 is ok for bands)
      (dotimes [_ bands]
        (if (> (rand) thresh) (q/fill 297 36 65 0.7)
          (q/fill 218 79 53 0.7))
        (let [x (rand-int (q/width)) y (rand-int (q/height))
              x' (+ x (* 50 (q/random-gaussian))) y' (+ y (* 50 (q/random-gaussian)))
              band
              (dband-vs (middle-field (fields/perlin-field 0.003) (pull-field 500 [400 400]))
                        70 3 [x y] [x' y'])]
          (q/begin-shape)
          (doseq [vs band]
            (apply q/vertex vs))
          (q/end-shape)
          #_(dband
                (avg-fields (fields/perlin-field 0.003)
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
    (q/image g1 20 20)))

(defn setup []
  (q/no-loop))

(defn spacedress [n-bands border?]
  (q/sketch 
   :size [800 800]
   :setup setup
   :settings #(q/smooth 16)
   :draw #(drawing n-bands border?)
   ;; try to remove overlapping bands  
   :drawa non-overlapping)) 

(comment (spacedress 120 false))
