(ns sketch.flowbands
  (:require [quil.core :as q]
            [util.fields :as f]
            [generative.flowfields :as fl]
            [clojure.data.json :as json]))

(def sanzo (clojure.edn/read-string (slurp "resources/sanzo-colors.edn")))
(def combos (->> (sanzo "combos")
                 (filter #(= (count (get % "id_colors")) 4))))
(defn flow-curve
  "Returns the coords that make a path with a flow field. No drawing is done"
  [field-fn n-steps step-len start-x start-y]
  (loop [i 1
         x start-x y start-y
         ret [[start-x start-y]]]
    (if (<= i n-steps)
      (let [V (mapv #(* step-len %) (field-fn x y))
            [x' y'] (mapv + [x y] V)]
        (recur (inc i) x' y' (conj ret [x' y'])))
      ret)))

(defn draw-curve [pts]
  (doseq [pair (partition 2 1 pts)]
    (let [[[x y] [x' y']] pair]
      (q/line x y x' y'))))

(defn draw-spots [size]
  (fn [pts] 
    (doseq [pair (partition 2 1 pts)]
      (let [[[x y] [x' y']] pair]
        (q/ellipse (/ (+ x x') 2) (/ (+ y y') 2) (/ size 2) (/ size 2)))))) 

(defn foo-band [fc1 fc2]
  (into fc1 (reverse fc2)))

(defn draw-band
  [field-fn n-steps step-len start-x start-y start-x' start-y']
  (let [side1 (flow-curve field-fn n-steps step-len start-x start-y)
        side2 (flow-curve field-fn n-steps step-len start-x' start-y')]
    ;(into side1 (reverse side2))
    (q/begin-shape)
    (doseq [vx (conj (into side1 (reverse side2)) (first side1))]
      (apply q/vertex vx))
    (q/end-shape)))
;; game plan
;; so different type of textures
;; flow lines with sloght color change
;; circles allong the flow
;; dont fill, leave the texture
;; use random placement (and random color/texture pick?)
;; explore proportions

(def draw-classic 
  (fn [palette] 
   (let [hsb-vals (shuffle (get palette "hsb"))]
     (apply q/background (hsb-vals 0)) 
     (q/stroke ((hsb-vals 0) 0) ((hsb-vals 0) 1) (+ -1 ((hsb-vals 0) 2)))
     (dotimes [i 300]
       (->> (flow-curve (f/perlin-field 5e-4) 900 5 
                        (rand-int (q/width))
                        (rand-int (q/height)))
            draw-curve))
     (apply q/fill (hsb-vals 1))
     (apply q/stroke (hsb-vals 1))
     (draw-band (f/perlin-field 5e-4) 100 5 100 100 150 180)
     (q/stroke ((hsb-vals 1) 0) ((hsb-vals 1) 1) (+ 4 ((hsb-vals 1) 2)))
     (q/no-fill)
     (let [x_ini 100 x_end 150 y_ini 100 y_end 180
           dir (->> (map - [x_end y_end] [x_ini y_ini])
                    (mapv #(/ % 10)))]
       (dotimes [i 10]
         (let [[d1 d2] (mapv + [x_ini y_ini] (mapv #(* (inc i) %) dir))]
           (->> (flow-curve (f/perlin-field 5e-4) 99 5 d1 d2)
                ((draw-spots 5))))))
     (q/no-stroke)        
     (apply q/fill (hsb-vals 1))
     (draw-band (f/perlin-field 5e-4) 100 5 700 100 650 80)
     (q/stroke ((hsb-vals 1) 0) ((hsb-vals 1) 1) (+ 4 ((hsb-vals 1) 2)))
     (let [x_ini 700 x_end 650 y_ini 100 y_end 80
           dir (->> (map - [x_end y_end] [x_ini y_ini])
                    (mapv #(/ % 10)))]
       (dotimes [i 8]
         (let [[d1 d2] (mapv + [x_ini y_ini] (mapv #(* (+ 1 i) %) dir))]
           (->> (flow-curve (f/perlin-field 5e-4) 99 5 d1 d2)
                ((draw-spots 5))))))
     (q/no-stroke)
     (apply q/fill (hsb-vals 2))
     (draw-band (f/perlin-field 5e-4) 100 5 500 500 450 380)
     (q/stroke ((hsb-vals 2) 0) ((hsb-vals 2) 1) (+ 1 ((hsb-vals 2) 2)))
     (q/no-fill)
     (let [x_ini 500 x_end 450 y_ini 500 y_end 380
           dir (->> (map - [x_end y_end] [x_ini y_ini])
                    (mapv #(/ % 10)))]
       (dotimes [i 10]
         (let [[d1 d2] (mapv + [x_ini y_ini] (mapv #(* (inc i) %) dir))]
           (->> (flow-curve (f/perlin-field 5e-4) 99 5 d1 d2)
                ((draw-spots 10))))))
     (q/no-stroke)
     (draw-band (f/perlin-field 5e-4) 100 5 700 700 650 580)
     (q/stroke ((hsb-vals 2) 0) ((hsb-vals 2) 1) (+ 1 ((hsb-vals 2) 2)))
     (let [x_ini 700 x_end 650 y_ini 700 y_end 580
           dir (->> (map - [x_end y_end] [x_ini y_ini])
                    (mapv #(/ % 10)))]
       (dotimes [i 10]
         (let [[d1 d2] (mapv + [x_ini y_ini] (mapv #(* (inc i) %) dir))]
           (->> (flow-curve (f/perlin-field 5e-4) 99 5 d1 d2)
                (draw-curve)))))
     (q/no-stroke)
     (apply q/fill (hsb-vals 3))
     (draw-band (f/perlin-field 5e-4) 100 5 600 800 550 580)
     (q/stroke ((hsb-vals 3) 0) ((hsb-vals 3) 1) (+ 2 ((hsb-vals 3) 2)))
     (let [x_ini 600 x_end 550 y_ini 800 y_end 580
           dir (->> (map - [x_end y_end] [x_ini y_ini])
                    (mapv #(/ % 10)))]
       (dotimes [i 10]
         (let [[d1 d2] (mapv + [x_ini y_ini] (mapv #(* (inc i) %) dir))]
           (->> (flow-curve (f/perlin-field 5e-4) 99 5 d1 d2)
                (draw-curve)))))
     (q/no-stroke)
     (draw-band (f/perlin-field 5e-4) 100 5 700 200 650 280)
     (q/stroke ((hsb-vals 3) 0) ((hsb-vals 3) 1) (+ 2 ((hsb-vals 3) 2)))
     (let [x_ini 700 x_end 650 y_ini 200 y_end 280
           dir (->> (map - [x_end y_end] [x_ini y_ini])
                    (mapv #(/ % 10)))]
       (dotimes [i 10]
         (let [[d1 d2] (mapv + [x_ini y_ini] (mapv #(* (inc i) %) dir))]
           (->> (flow-curve (f/perlin-field 5e-4) 99 5 d1 d2)
                (draw-curve))))))))

(defn draw-bands
  [n-bands pallete]
  (let [hsb-vals (shuffle (get pallete "hsb"))
        bg (hsb-vals 0)]
    (apply q/background bg)
    (q/stroke (bg 0) (bg 1) (+ 2 (bg 2)))
    (dotimes [i 300]
      (let [x (rand-int (q/width)) y (rand-int (q/height))]
        (->> (flow-curve (f/perlin-field 5e-4) 900 5 x y)
             draw-curve)))
    (dotimes [i n-bands]
      (let [x (rand-int (q/width)) y (rand-int (q/width))
            x' (+ x (* (q/random-gaussian) 70)) y' (+ y (* (q/random-gaussian) 70))
            dir (->> (map - [x' y'] [x y]) (mapv #(/ % 10)))
            color (rand-nth (subvec hsb-vals 1))]
        (when (< 0.1 (rand))
          (q/no-stroke)
          (apply q/fill color)
          (draw-band (f/perlin-field 5e-4) 100 4 x y x' y'))
        (let [texture (rand-nth [:circle :line :none])]
          (case texture
            :circle
            (do
              (q/no-fill)
              (q/stroke (color 0) (color 1) (+ 2 (color 2)))
              (dotimes [i 9]
                (let [[d1 d2] (mapv + [x y] (mapv #(* (inc i) %) dir))]
                  (->> (flow-curve (f/perlin-field 5e-4) 99 5 d1 d2)
                       ((draw-spots 10))))))
            :line
            (do
              (q/stroke (color 0) (color 1) (+ 2 (color 2)))
              (dotimes [i 9]
                (let [[d1 d2] (mapv + [x y] (mapv #(* (inc i) %) dir))]
                  (->> (flow-curve (f/perlin-field 5e-4) 99 5 d1 d2)
                       (draw-curve)))))
            nil))))))

(declare simple-bands)
(q/defsketch simple-bands
  :title "Bands"
  :setup (fn []
           (q/no-stroke)
           (q/no-loop)
           (def pallete (rand-nth combos))
           (q/color-mode :hsb 359 100 100 1))
  :settings #(q/smooth 16)
  :draw #(draw-bands 12 pallete)
  :size [800 800]
  :renderer :java2d)

(comment
  (quil.applet/with-applet sketch.flowbands/simple-bands
     (let [hash (subs (str (random-uuid)) 0 5)]
       (q/save (str "out/flowbands/bands_" hash ".png"))))) ;
