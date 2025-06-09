(ns roulettes.cylocycloid
  (:require [quil.core :as q]
            [clojure.math :as math :refer [PI]]))

(defn cyclocycloid [R r d t]
  (let [Rr (+ R r) qr (/ Rr r)]
    [(- (* Rr (math/cos t))
        (* d (math/cos (* qr t))))
     (- (* Rr (math/sin t))
        (* d (math/sin (* qr t))))]))

(defn draw-cyclocycloid 
  [R r d step]
  (q/no-loop)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/color-mode :hsb 360 1 1 1)  
  ;; TODO: Figure out the period
  (dotimes [i (inc (/ (* 2 (+ (numerator (clojure.lang.Numbers/toRatio (/ R (abs r))))
                             (denominator (clojure.lang.Numbers/toRatio (/ R (abs r))))))
                     step))] 
    (let [t (* i step)
          t' (+ t step)]
      (q/stroke (mod (q/frame-count) 360) 0.5 0.5 1)
      (let [[x   y] (cyclocycloid R r d t)
            [x' y'] (cyclocycloid R r d t')]
        (q/line x y x' y')))))

(defn setup []
  (q/frame-rate 30)
  (q/background 0)
  (q/stroke 128 30 20))

(defn sketch-cyclocycloid 
  [R r d step]
  (let [t (atom 0)]
   (q/sketch
     :title "sketch"
     :settings #(q/smooth)
     :key-pressed (fn [] (if (q/looping?) (q/no-loop) (q/start-loop)))
     :setup setup
     :draw (fn [] 
             (q/fill 0) (q/no-stroke)
             (q/rect 0 0 40 100)
             (q/fill 256)
             (q/text (format "t=%.1f" (* step (dec (q/frame-count)))) 0 0 40 100)
             (q/translate (/ (q/width) 2) (/ (q/height) 2))
             (q/color-mode :hsb 360 1 1 1)  
             ;(q/stroke 188 0.01)
             ;(q/no-fill)
             ;(q/ellipse 0 0 (* 2 R) (* 2 R))
             ;(q/ellipse (+  R r ) 0 (* 2 r) (* 2 r))
             (let [t' (+ @t step)
                   [x   y] (cyclocycloid R r d @t)
                   [x' y'] (cyclocycloid R r d t')]
                 (q/stroke (mod (q/frame-count) 360) 0.5 0.5 1)
                 (q/line x y x' y')
                 (reset! t t')))
     :size [1000 1000])))

(comment 
  (let [r 50 R (* (abs r) (/ 2 1)) d 0.8] 
    (sketch-cyclocycloid R r (* d r) 0.1))
  (sketch-cyclocycloid 180 60 60 0.1)
  (q/defsketch example
   :title "Spirograph"
   :settings #(q/smooth 2)
   :setup setup
   :draw #(draw-cyclocycloid 180 90 90 0.1)
   :size [800 800]))

