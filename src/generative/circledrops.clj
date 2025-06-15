(ns generative.circledrops
  (:require [quil.core :as q]
            [clojure.math :as math]
            [clojure.data.json :as json]
            [util.core :refer [local-context hex-to-rgb]]))

(def params 
  (let [Width 800
        Height 800
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

(def combos ((json/read-str (slurp "resources/sanzo-colors.json")) "combos"))

(defn draw-curve-with-point-field 
  "Draws a curve given start coordinates, number of segments, segment lenght initial stroke
   Takes a function representing a flow field at point"
  [start-x start-y n-segments step-length field-fun thickness color]
  (q/stroke-weight thickness)
  (q/stroke (conj color (- 255 (* 8 n-segments)))) ;; fades-in each iteration
  ;(apply q/stroke (conj color (rand-nth [255 127 63]))) ;; applies random alpha each call
  (when (> n-segments 0)
    (let [x start-x y start-y
          angle (field-fun x y)
          next-x (+ x (* step-length (math/cos angle)))
          next-y (+ y (* step-length (math/sin angle)))
          middle-x (/ (+ next-x x) 2)
          middle-y (/ (+ next-y y) 2)]
          ; replace with function?
      (if (and (>= x 0) (>= y 0)
               (<= x (:Width params)) (<= y (:Height params)))
         ;(q/stroke-weight (+ (int (* 10 (/ row (:num-rows params)))) (rand-int 5)))
         (do (q/line start-x start-y next-x next-y)
             (draw-curve-with-point-field 
               middle-x middle-y 
               (dec n-segments) step-length
               field-fun thickness color))
         (draw-curve-with-point-field
           middle-x middle-y
           (dec n-segments) step-length
           field-fun thickness color)))))

(defn iterate-drawing-circle
  [times]
  (let [pallete (mapv hex-to-rgb ((rand-nth combos) "hex"))]
    (q/stroke-cap :square)
    (q/stroke-join :bevel)
    (doseq [n (range times)]
    ; (apply q/stroke (conj (rand-nth pallete) (rand-nth [255 200 145])))
     ;(q/line x y (+ x 30) (+ y 40)))
     (draw-curve-with-point-field 
       (rand-int (:Width params)) (rand-int (:Height params))
       (rand-int 90)  10 
       (fn [x y] (+ ;(* 0.5 (q/noise x y)) 
                    (-  (math/atan2 (- (/ (:Width params) 2)  x) (- (/ (:Height params) 2) y)))))
       (+ 2 (rand-int 90))
       (rand-nth pallete)))
    ;(q/save "circle_c.png")
    (q/no-loop)))

(defn setup []
  (q/frame-rate 8)
  (q/background 50)
  ;(q/background 200)
  ;(q/stroke 0)
  (q/stroke-weight 1))
  ;(doseq [n (range 0 500)]
  ;  (perlin-curve))
  ;(q/no-loop))

(comment 
  (q/defsketch example
    :title "Flow Fields"
    :display 1
    :settings #(q/smooth)
    :setup setup
    :draw #(iterate-drawing-circle 40)
    :size [800 800]
    :features [:resizable]
    :renderer :java2d))

(comment 
  (quil.applet/with-applet generative.circledrops/example 
    (q/save (str "out/circles_" (subs (str (random-uuid)) 0 5) ".png")))) ;
