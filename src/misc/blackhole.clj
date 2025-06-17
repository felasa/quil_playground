;; Credit: @XoDev https://x.com/XorDev/status/1897042877626421642

(ns misc.blackhole
  (:require [quil.core :as q]
            [clojure.math :as math]))

(defn bh-intensity [W H x y]
  (let [ax (/ (+ (* 2 x) (- W)) H)
        ay (/ (+ (* 2 y) (- H)) H)
        s1 (math/sqrt (+ (* ax ax) (* ay ay)))
        s2 -0.5
        s3 (/ (* 0.01 H) (+ (* 2 (- x y)) H (- W)))]
    (float (/ 0.1 (abs (+ s1 s2 s3))))))              

(defn setup []
  (q/frame-rate 60)
  (q/background 0)
  (q/no-loop))

(defn draw-bh
  [W H]
  (doseq [x (range W) y (range H)]
    (let [I (* 255 (bh-intensity W H x y))]
      (q/set-pixel (- H y) x (q/color I))))) 
  ;(q/save "out/horizon.png"))

(def W 800)
(def H 800)

(comment 
  (q/defsketch bh
    :title "BH"
    :display 1
    :settings #(q/smooth 8)
    :setup setup
    :draw #(draw-bh W W)
    :bg-color 0
    :size [W H]
    :features [:resizable]
    :renderer :opengl)) ;:java2d))

(comment 
  (quil.applet/with-applet
    misc.blackhole/bh 
    (q/save "out/horizon.png"))) ;
