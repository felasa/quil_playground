(ns sketch.circle-density
  (:require [quil.core :as q]
            [genartlib.poisson-disc :as pois]))

(declare sketch)
(def palletes
  (->> 
    (slurp "resources/sanzo-colors.edn")
    (clojure.edn/read-string)
    :combos
    (filter #(= (count (:hsb %)) 2))
    (map #(:hsb %))))

(defn draw []
  (q/noise-seed (rand-int 10000))
  (q/color-mode :hsb 359 100 100 1.0)
  ;(q/background 0.0)
  ;(q/fill 1.0)
  (q/no-stroke)
  ;(q/ellipse-mode :radius)
  ;(q/ellipse 400 400 100 100)
  (let [D (+ 10 (rand-int 20))
        pts (pois/poisson-disc-sample D 1000 (/ D 2) (- (q/width) D) (/ D 2) (- (q/width) D))
        noise-scale (rand-nth [0.01 0.005 0.001])
        bgh (rand-int 359) fgh (mod (+ bgh 180) 360)
        bg [bgh 100 0] fg [fgh 0 100]]
        ;[bg fg] (rand-nth palletes)]
    (apply q/fill fg)
    (apply q/background bg)
    (doseq [[x y] pts]
            ;x (map #(+ (/ D 2) %) (range 0 (- (q/width) D) D))
            ;y (map #(+ (/ D 2) %) (range 0 (- (q/height) D) D))]
     (let [sx (* x noise-scale) sy (* y noise-scale)
           n (q/noise sx sy)
           d (* n D)] 
      ;(q/ellipse x y (* 15 (q/noise sx sy)) (* 10 (q/noise sx sy)))
      (q/ellipse x y d d)))))
      ;(q/set-pixel x y (q/color (q/noise sx sy))))))

(defn multi-draw []
  (dotimes [i 50]
    (draw)
    (let [fname (str "out/circle_density/bw/circle_density_" (format "%03d" (+ 50 i)) ".png")]
      (q/save fname))))

(q/defsketch sketch
  :size [800 800]
  :setup (fn [] (q/no-loop))
  :draw multi-draw)
 


