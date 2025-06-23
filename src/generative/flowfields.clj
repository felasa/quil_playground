(ns generative.flowfields
  (:require [quil.core :as q]
            [clojure.math :as math]
            [util.core :refer [local-context]]))

;; TODO: 
;; - pass cruve-fn to draw
;; - fn to handle next curve position (avoid collisions/spacing)
;; - draw circle instead of line segment
;; - color handling

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

(defn scale-coord
  "snap value to a grid defined by resolution"
  [max-value resolution value]
  (* (int (/ value max-value resolution)) max-value resolution))

(defn perlin-noise
  [x y scale]
  (q/noise (* scale x) (* scale y)))

(defn perlin-field
  [scale x y]
  (* 2 math/PI (perlin-noise x y scale)))

(defn grid-field
  "'Snaps' field-fn to resolution produced by grid-fn"
  [field-fn grid-fn]
  (fn [x y] (field-fn (grid-fn x) (grid-fn y))))
  
(def pallete [[0xcc 0x12 0x36] [0xfd 0xbe 0x68] [0x00 0x97 0x8c]])

(defn draw-curve-with-field
  "Draws a curve given start coordinates, number of segments, segment lenght initial stroke
   Takes a function representing a flow field at a downsampled grid. column, row coordinates"
  [field-fun n-segments step-length start-x start-y]
  (loop [n n-segments
         x start-x y start-y]
    (if (> n 0)
      (let [angle (field-fun x y)
            next-x (+ x (* step-length (math/cos angle)))
            next-y (+ y (* step-length (math/sin angle)))]
         (q/line x y next-x next-y)
         (recur (dec n) 
                next-x next-y))
      nil)))

(defn drw
  "draws n-curves curves using field-fn"
  [field-fn n-curves]
  (q/no-loop)
  ;(q/stroke 2)
  (q/stroke-weight 3)
  ;(q/point col row)
  (q/stroke-weight 1)
  (dotimes [_ n-curves]
    (let [col (rand-int (q/width)) row (rand-int (q/height))
          color (rand-nth pallete)]
      (q/stroke (color 0) (color 1) (color 2) 255)
      (draw-curve-with-field field-fn 500 5
                             col row))))

(defn setup []
  (q/frame-rate 500)
  (q/background 255)
  (q/stroke 0)
  (q/stroke-join :round))

(defn draw-animated 
  [field-fn step-size n-segments]
  (let [x (atom 400) 
        y (atom 400)
        segment-count (atom 0)]
    (q/sketch
      :draw 
      (fn []
        (if (<= @segment-count n-segments)
          (let [theta (field-fn @x @y)
                x' (+ @x (* step-size (math/cos theta)))
                y' (+ @y (* step-size (math/sin theta)))]
            (q/line @x @y x' y')
            (reset! x x')
            (reset! y y')
            (swap! segment-count inc)) 
          (do (reset! x (rand-int (q/height)))
              (reset! y (rand-int (q/width)))
              (reset! segment-count 0)))) 
      :settings #(q/smooth 8)
      :setup (fn [] 
              (q/frame-rate 500)
              (q/background 255)
              (q/stroke 0)
              (q/stroke-join :round))
      :size [800 800])))

(comment
  (draw-animated (partial perlin-field 0.07) 2 150)
  (draw-animated (grid-field (partial perlin-field 0.005) 
                             (partial scale-coord 800 (/ 4)))
                 10 250)
  (q/defsketch example
    :title "Title"
    ;:display 1
    :settings #(q/smooth)
    :setup setup
    ;:draw (fn [] (q/line 0 0 800 800))
    :draw #(drw (grid-field (partial perlin-field 0.005) 
                            (partial scale-coord 800 (/ 20)))
               900)
    :size [800 800]
    :features [:resizable]
    :renderer :java2d))

;Example drawing composing field with grid 
;(drw (grid-field (partial perlin-field 0.005 
;                 (partial scale-coord 800 (/ 20))
;     900))
(comment
  (quil.applet/with-applet generative.flowfields/example
    (q/save "flowfield_005_20_900.png"))) ;

