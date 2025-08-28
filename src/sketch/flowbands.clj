(ns sketch.flowbands
  (:require [quil.core :as q]
            [quil.applet]
            [util.fields :as f]
            [clojure.edn]))

(def sanzo (clojure.edn/read-string (slurp "resources/sanzo-colors.edn")))
(def combos (->> (sanzo :combos)
                 (filter #(= (count (get % :id_colors)) 4))))
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

;todo: make it work
(defn perpendicular-segment [[x y] [x' y']]
  (let [[dx dy] (mapv - [x' y'] [x y])
        d' [(- dy) dx]]
    (mapv + d' [x y])))

;same
(defn texture-perpendicular
  [pts]
  (doseq [pair (partition 2 1 pts)]
    (let [[p p'] pair
          [[px py] [px' py']] (perpendicular-segment p p')]
      (q/line px py px' py'))))

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

(defn draw-bands
  [n-bands pallete]
  (let [hsb-vals (shuffle (get pallete :hsb))
        bg (hsb-vals 0)]
    (apply q/background bg)
    (q/stroke (bg 0) (bg 1) (+ 2 (bg 2)))
    (dotimes [_ 300]
      (let [x (rand-int (q/width)) y (rand-int (q/height))]
        (->> (flow-curve (f/perlin-field 5e-4) 900 5 x y)
             ;texture-perpendicular)))
             draw-curve)))
    (dotimes [_ n-bands]
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
            nil))))
   ; make a region negative
   (doseq [x (range 100 700) y (range 200 600)]
     (when (<= (+ (q/sq (- x 400)) (q/sq (- y 400))) (q/sq 250))
       (let [pxl (q/get-pixel x y)
             npxl (bit-xor 0xffffff pxl)]
         (q/set-pixel x y npxl))))
   #_(doseq [x (range 200 600) y (range 200 600)]
        (let [pxl (q/get-pixel x y)
              npxl (bit-xor 0xffffff pxl)]
          (q/set-pixel x y npxl)))))

(declare simple-bands)
(q/defsketch simple-bands
  :title "Bands"
  :setup (fn []
           (q/no-stroke)
           (q/no-loop)
           (def pallete (rand-nth combos))
           (q/color-mode :hsb 359 100 100 1))
  :settings #(q/smooth)
  :draw #(draw-bands 12 pallete)
  :size [800 800]
  :renderer :opengl)

(comment
  (quil.applet/with-applet sketch.flowbands/simple-bands
     (let [hash (subs (str (random-uuid)) 0 5)]
       (q/save (str "out/flowbands/bands_" hash ".png"))))) ;
