(ns dev.rgb
  (:require [quil.core :as q]
            [shapes.polygons :as poly]
            [util.curve :refer [draw-shape]]
            [util.misc :refer [save-sketch]]))

(declare sketch)

(defn draw []
  (doseq [x (range (q/width)) y (range (q/height))]
    (let [color-key (rand-nth [:red :green :blue])
          color (case color-key
                   :red   (q/color 255 128 128)
                   :green (q/color 128  255 128)
                   :blue  (q/color 128 128 255))]
      #_(case color-key
          :red   (q/stroke 255 0 0 128)
          :green (q/stroke 0 255 0 128)
          :blue  (q/stroke 0 0 255 128))
      (q/set-pixel x y color)
      ;(q/point x y)
      #_(case color
          :red   (q/set-pixel x y (q/color 255 0 0 4))
          :green (q/set-pixel x y (q/color 0 255 0 4))
          :blue  (q/set-pixel x y (q/color 0 0 255 4))))))
(defn triangle [x y scale]
  (draw-shape (map #(mapv + [x y] %) (poly/close-path (poly/n-gon scale 3)))))

(defn cross [x y width]
  (let [x (- x (/ width 2))
        y (- y (/ width 2))]
    (q/line x y (+ x width) (+ y width))
    (q/line (+ x width) y x (+ y width))))
(format "%x" 110)
(+ 0xFF000000 0xFE)

(defn rgb-offset [shape-fn x y & args]
   (q/no-fill)
   (q/stroke-weight 9)
   (q/stroke 255 0 0 110)
   ;(q/fill 255 0 0 100)
   (apply shape-fn (+ x (* 4.2 (q/random-gaussian))) (+ y (* 4.2 (q/random-gaussian))) args)
   (q/stroke 0 255 0 110)
   ;(q/fill 0 255 0 100)
   (apply shape-fn (+ x (* 4.2 (q/random-gaussian))) (+ y (* 4.2 (q/random-gaussian))) args)
   (q/stroke 0 0 255 110)
   ;(q/fill 0 0 255 100)
   (apply shape-fn (+ x (* 4.2 (q/random-gaussian))) (+ y (* 4.2 (q/random-gaussian))) args))
  

(q/defsketch sketch
  :renderer :java2d
  :size  [800 800]
  :setup (fn [] (q/no-loop)
           (q/frame-rate 10))
  :draw #(do (q/background 50)
             (dotimes [_ 1] (draw))
             (q/stroke-cap :project)
             (q/rect-mode :center)
             #_(doseq [i (range 6)] 
                 (rgb-offset q/rect
                             (+ 400 (* 20 i)) (+ 400 (* 20 i))
                             (- 500 (* 4 20 i)) (- 500 (* 4 20 i))))
             ;(rgb-offset q/ellipse 400 400 500 500)))
             ;(rgb-offset q/rect 400 400 500 500)))
             ;(rgb-offset triangle 400 400 500))) 
             (rgb-offset cross 400 400 500)))

  
(comment
  (quil.applet/with-applet dev.rgb/sketch
    (q/save (str "out/rgb/rgb_" (subs (str (random-uuid)) 0 5) ".png")))) ;
