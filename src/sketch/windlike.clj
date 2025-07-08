(ns sketch.windlike
  (:require [quil.core :as q]
            [clojure.math :as math]
            [clojure.data.json :as json]
            [clojure.java.shell :refer [sh]]))

(def combos ((json/read-str (slurp "resources/sanzo-colors.json")) "combos"))
(defn perlin-noise3
  [scale]
  (fn [x y z] (q/noise (* scale x) (* scale y) (* scale z))))

(defn perlin-field3
  [scale]
  (fn [x y z]
    (let [angle (* q/TWO-PI ((perlin-noise3 scale) x y z))
          dx (math/cos angle) dy (math/sin angle)]
      [dx dy])))

(defn curl-field3
  [scale]
  (fn [x y t]
    (let [delta 0.1
          dfy0 ((perlin-noise3 scale) x (- y delta) t)
          dfy1 ((perlin-noise3 scale) x (+ y delta) t)
          dfx0 ((perlin-noise3 scale) (- x delta) y t)
          dfx1 ((perlin-noise3 scale) (+ x delta) y t)
          [vx vy] [(/ (- dfy1 dfy0) (* 2 delta))
                   (/ (- dfx0 dfx1) (* 2 delta))]
          N (q/mag vx vy)]
      (mapv #(/ % N) [vx vy]))))

(defn draw-curve-with-field3
  [field-fn n-segments step-length start-x start-y t]
  (loop [n n-segments
         x start-x y start-y]
    (if (> n 0)
      (let [;angle (field-fn x y)
            [dx dy] (field-fn x y t)
            [x' y'] (mapv + [x y] (map #(* step-length %) [dx dy]))]
        (q/line x y x' y')
        (recur (dec n)
               x' y'))
      nil)))

(declare wind)
(q/defsketch wind
  :title "Wind"
  :settings #(q/smooth 8)
  :setup (fn [] (q/frame-rate 24) (q/noise-detail 4 0.5)
                (def id (subs (str (random-uuid)) 0 8))
                (def seeds [(System/currentTimeMillis) (System/currentTimeMillis)])
                (def pal (->> combos (filter #(= 3 (count (% "id_colors")))) 
                              (rand-nth)
                              (#(get % "hsb"))
                              shuffle))
                (def bg (pal 0))
                (def fg1 (pal 1))
                (def fg2 (pal 2)) 
                (def xss0 (vec (for [i (range 200)] (rand-int (q/width)))))
                (def yss0 (vec (for [i (range 200)] (rand-int (q/height)))))
                (def xss1 (vec (for [i (range 100)] (rand-int (q/width)))))
                (def yss1 (vec (for [i (range 100)] (rand-int (q/height))))))
  :size [800 800]
  :renderer :opengl
  ;not working
  ;:on-close 
  #_(sh "ffmpeg" 
      "-framerate" "10" 
      "-i" (str "out/animations/" id "/wind-%05d.png")
      "-c:v" "libx265" 
      "-pix_fmt" "yuv420p"
      (str "out/animations/" id "/animation.mp4"))
  :draw #(do
           (q/color-mode :hsb 359 100 100 1)
           (apply q/background bg)
           (q/stroke-cap :square)
           (q/stroke-weight 1)
           (q/noise-seed (seeds 1))
           (apply q/stroke fg1) 
           (dotimes [i 200]
             (draw-curve-with-field3
                 ;(curl-field3 0.005)
              (comp (fn [v] (mapv - v)) (perlin-field3 0.002))
              200
              5
              (xss0 i) (yss0 i) (* 1 (q/frame-count))))
           (apply q/stroke fg2) 
           (q/noise-seed (seeds 0))
           (q/stroke-weight 2)
           (dotimes [i 100]
             (draw-curve-with-field3
                 ;(curl-field3 0.005)
              (perlin-field3 0.01)
              200
              5
              (xss1 i) (yss1 i) (* 0.3 (q/frame-count))))
           (q/save-frame (str "out/animations/" id "/wind-#####.png"))))

