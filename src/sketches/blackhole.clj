;; Credit: @XoDev https://x.com/XorDev/status/1897042877626421642
(ns sketches.blackhole
  (:require [quil.core :as q]
            [quil.applet]))

(defn bh-intensity [W H x y]
  (let [ax (/ (+ (* 2 x) (- W)) H)
        ay (/ (+ (* 2 y) (- H)) H)
        s1 (Math/sqrt (+ (* ax ax) (* ay ay)))
        s2 -0.5
        s3 (/ (* 0.01 H) (+ (* 2 (- x y)) H (- W)))]
    (float (/ 0.1 (abs (+ s1 s2 s3))))))              

(defn setup []
  (q/background 0)
  (q/no-loop))

(defn draw-bh
  [W H]
  (q/color-mode :hsb 360 1 1 1)
  (doseq [x (range W) y (range H)]
    (let [x' (- x (/ W 2)) y' (- y (/ H 2))
          i (bh-intensity W H x y)
          I (* 255 i)]
      (q/set-pixel (- H y) x 
                   ;(q/color I) 
                   (q/color (+ 180 (Math/toDegrees (Math/atan2 y' x'))) 1 (* i i)))))) 
(declare bh)
(q/defsketch bh
  :title "BH"
  :settings #(q/smooth 8)
  :setup setup
  :draw #(draw-bh 800 800)
  :bg-color 0
  :size [800 800]
  :renderer :opengl) ;:java2d))

(comment 
  (quil.applet/with-applet
    bh
    (q/save "out/horizon_hsv_hard.png"))) ;
