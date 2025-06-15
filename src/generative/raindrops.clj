(ns generative.raindrops
  (:require [quil.core :as q]
            [clojure.math :as math]
            [util.core :refer [local-context]]))

(def params 
  (let [Width 1000
        Height 1000
        resolution-factor 0.01
        left-x (* Width -0.5)
        right-x (* Width 1.5)
        top-y (* Height -0.5)
        bottom-y (* Height 1.5)
        resolution (* Width resolution-factor)
        num-cols (/ (- right-x left-x) resolution)
        num-rows (/ (- bottom-y top-y) resolution)
        perlin-scale 0.005
        lc (local-context)]
    (update-keys lc keyword)))
    
(defn perlin-noise 
  [col row scale]
  ;(. papp noise (* scale col) (* scale row))))
  (q/noise (* scale col) (* scale row)))

(defn perlin-field
  [column row]
  (* 2 math/PI (perlin-noise column row (:perlin-scale params))))

(defn draw-curve-with-field 
  "Draws a curve given start coordinates, number of segments, segment lenght initial stroke
   Takes a function representing a flow field at a downsampled grid. column, row coordinates"
  [start-x start-y n-segments step-length field-fun thickness]
  (q/stroke-weight thickness)
  (when (> n-segments 0)
    (let [x start-x y start-y
          x-offset (- x (:left-x params)) y-offset (- y (:top-y params))
          column (int (/ x-offset (:resolution params)))
          row (int (/ y-offset (:resolution params)))            
          angle (field-fun column row)
          next-x (+ x (* step-length (math/cos angle)))
          next-y (+ y (* step-length (math/sin angle)))
          ; replace with function?
          new-thick (max (+ (- (rand-int 5) 4) 
                            ;(int (* 0 (/ row (:num-rows params)))) 
                            thickness) 1)]
      (if (and (>= column 0) (>= row 0)
               (<= column (:num-cols params)) (<= row (:num-rows params)))
         ;(q/stroke-weight (+ (int (* 10 (/ row (:num-rows params)))) (rand-int 5)))
         (do (q/line start-x start-y next-x next-y)
             (draw-curve-with-field next-x next-y (dec n-segments) step-length
                                    field-fun new-thick))
         (draw-curve-with-field next-x next-y (dec n-segments) step-length
                                field-fun new-thick)))))

(def pallete [[0xff 0xef 0xb0 0xff] [0xc7 0x5c 0x52 0xff] [0xb7 0x96 0x96 0xff]])

(defn setup []
  (q/frame-rate 8)
  (q/background 50)
  ;(q/stroke 0)
  (q/stroke-weight 1)
  (q/stroke-cap :round))
  ;(doseq [n (range 0 500)]
  ;  (perlin-curve))
  ;(q/no-loop))

(defn iterate-drawing
  [times]
  (doseq [n (range 0 times)]
    (apply q/stroke (rand-nth pallete))
    (draw-curve-with-field 
      (rand-int (:right-x params)) (rand-int (:bottom-y params))
      50 6 perlin-field 20))
  ;(q/save "test.png")
  (q/no-loop))

(comment 
  (q/defsketch example
    :title "Rain"
    :display 1
    :settings #(q/smooth 32)
    :setup setup
    :draw #(iterate-drawing 200)
    :bg-color 50
    :size [1024 800]
    ;:features [:exit-on-close]))
    :renderer :java2d))

(comment 
  (quil.applet/with-applet
    generative.raindrops/example 
    (q/save (str "out/raindrops_" (subs (str (random-uuid)) 0 5) ".png")))) ;
