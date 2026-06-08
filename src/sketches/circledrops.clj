(ns sketches.circledrops
  (:require [quil.core :as q]
            [util.core :refer [hex-to-rgb]]
            [util.fields :refer [circle-field]]
            [quil.applet]))

(def combos (:combos (read-string (slurp "resources/sanzo-colors.edn"))))

(defn draw-curve-with-point-field 
  "Draws a curve given start coordinates, number of segments, segment lenght initial stroke
   Takes a function representing a flow field at point"
  [start-x start-y n-segments step-length field-fun thickness color]
  (q/stroke-weight thickness)
  (q/stroke (conj color (- 255 (* 8 n-segments)))) ;; fades-in each iteration
  ;(apply q/stroke (conj color (rand-nth [255 127 63]))) ;; applies random alpha each call
  (when (> n-segments 0)
    (let [x start-x y start-y
          [next-x next-y] (map + [x y] (map * (repeat 2 step-length) (field-fun [x y])))
          [middle-x middle-y] (map #(/ % 2) (map + [x y] [next-x next-y]))]
          ; replace with function?
      ;(q/stroke-weight (+ (int (* 10 (/ row (:num-rows params)))) (rand-int 5)))
      (q/line start-x start-y next-x next-y)
      (draw-curve-with-point-field 
        middle-x middle-y 
        (dec n-segments) step-length
        field-fun thickness color))))

(defn iterate-drawing-circle
  [times]
  (let [pallete (mapv hex-to-rgb ((rand-nth combos) :hex))]
    (q/stroke-cap :square)
    (q/stroke-join :bevel)
    (dotimes [_ times]
    ; (apply q/stroke (conj (rand-nth pallete) (rand-nth [255 200 145])))
     ;(q/line x y (+ x 30) (+ y 40)))
     (draw-curve-with-point-field 
       (rand-int (q/width)) (rand-int (q/height))
       (rand-int 90) 10 
       (partial circle-field (q/width) (q/height))
       #_(fn [x y] (+ ;(* 0.5 (q/noise x y)) 
                      (-  (math/atan2 (- (/ (q/width)  2) x) (- (/ (q/height) 2) y)))))
       (+ 2 (rand-int 90))
       (rand-nth pallete)))))

(defn setup []
  (q/background 50)
  ;(q/background 200)
  ;(q/stroke 0)
  (q/stroke-weight 1)
  (q/no-loop))

(declare circledrops)
(q/defsketch circledrops
  :title "Circle drops"
  :display 1
  :settings #(q/smooth)
  :setup setup
  :draw #(iterate-drawing-circle 60)
  :size [800 800]
  :features [:resizable]
  :renderer :java2d)

(comment 
  (quil.applet/with-applet sketches.circledrops/circledrops 
    (q/save (str "out/circles_" (subs (str (random-uuid)) 0 5) ".png")))) ;
