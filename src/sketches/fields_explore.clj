(ns sketches.fields-explore
  (:require [quil.core :as q]
            [quil.applet]
            [util.fields :as f]))

(declare sketch)
;; parameters: draw on edges draw random pieces of field, what color, scale of field
(defn draw [scale]
  ;(q/noise-detail 16 0.8)
  (q/color-mode :hsb 359 1 1 1)
  (doseq [x (range (q/width)) y (range (q/height))]
    (let [value ((f/perlin-noise scale) [x y])
          [x' y'] ((f/curl-field scale) x y)
          dec-round (/ (Math/round (* 10 value)) 10)
          ;color (cond (< value 0.33) (q/color 1 0 0) (<= value 0.66) (q/color 0 0 1) :else (q/color 0 1 0))]
          color (q/color (+ 220 (* 2 (rand))) 0.8 dec-round)
          N (q/mag x' y') [nx ny] (if (< N 1e-100) [x' y'] [(/ x' N) (/ y' N)])]
      (q/set-pixel x y color)
      ;; density of lines and lenght of them
      (q/stroke 255)
      (when (< (rand) 0.01) (q/line x y (+ x (* x' 5e3)) (+ y (* y' 5e3)))) 
      ;; want to draw near edges
      (when (and (not= 0 N) 
                 ;(< (rand) 0.99) 
                 (or (< 0.049 value 0.0501)
                     (< 0.149 value 0.1501)
                     (< 0.249 value 0.2501)
                     (< 0.349 value 0.3501)
                     (< 0.449 value 0.4501)
                     (< 0.549 value 0.5501) 
                     (< 0.649 value 0.6501) 
                     (< 0.749 value 0.6501) 
                     (< 0.849 value 0.8501) 
                     (< 0.949 value 0.9501))) 
        (q/stroke 0)
        (q/line x y (+ x (* nx 1e1)) (+ y (* ny 1e1))))))) 
      ;(check-field scale 10))))

(defn test-field [scale]
  (dotimes [_ 100]
    (let [x (rand-int (q/width)) y (rand-int (q/height))
          [dx dy] ((f/curl-field scale) x y)]
      (q/line x y (+ x (* 1e4 dx)) (+ y (* 1e4 dy))))))

(defn check-field [scale seg-len]
  ;(q/noise-detail 16 0.5)
  (loop [x 400 y 400 iter 1]
    (when (<= iter 500)
      (let [[dx dy] ((f/curl-field scale) x y)
            x' (+ x (* seg-len dx)) y' (+ y (* seg-len dy))]
        (println [dx dy])
        (q/line x y x' y')
        (recur x' y' (inc iter))))))

(q/defsketch sketch
  :size [800 800]
  ;:settings (q/smooth)
  :setup (fn [] (q/no-loop))
  :draw #(draw 0.005))
  ;:dra #(test-field 0.003)
  ;:dra #(check-field 0.005 10))

(comment
  (quil.applet/with-applet sketch.fields-explore/sketch 
    (q/save (str "out/cfield-experiment/" (subs (str (random-uuid)) 0 5) ".png")))) ;
