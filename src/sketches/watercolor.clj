;; todo: could use cleanup
(ns sketches.watercolor
  "Tries to implement https://www.tylerxhobbs.com/words/a-guide-to-simulating-watercolor-paint-with-generative-art"
  (:require [quil.applet]
            [quil.core :as q]
            [util.shapes :as poly]
            [util.shapes :refer [blob]]
            [util.transform :as util]
            [util.masking :refer [mask-w-alpha]]
            [util.random :refer [gauss]]
            [util.core :refer [hex-to-rgb]]))

(defn setup []
  (q/no-loop)
  ;(q/background 250 0)
  (q/frame-rate 10)
  (q/set-state! :l1 (q/create-graphics (q/width) (q/height))
                :l2 (q/create-graphics (q/width) (q/height))
                :alpha (q/create-graphics (q/width) (q/height))))

(defn draw-shape
  [vxs]
  (q/begin-shape)
  (doseq [vx vxs]
    (apply q/vertex vx))
  (q/end-shape))

(defn working []
  (q/background 255)
  (dotimes [_ 150]
    (let [alpha 8
          texture-layer (q/create-graphics (q/width) (q/height))
          shape-layer   (q/create-graphics (q/width) (q/height))]
      (q/with-graphics shape-layer
        (q/background 255 0)
        (q/no-stroke)
        (q/fill 0 alpha)
        (draw-shape (blob [300 300] 300 [20 30 40 50 60 70 80 90 100] 0.8 5)))
      (q/with-graphics texture-layer
        (q/background 255 0)
        (q/no-stroke)
        (q/fill 255 0 0)
        (dotimes [_ 900]
          (q/ellipse (rand-int (q/width)) (rand-int (q/height)) 50 50))
        (q/mask-image shape-layer))
      (q/image texture-layer 0 0))))
      ;(q/blend texture-layer 0 0 (q/width) (q/height) 0 0 (q/width) (q/height) :darkest))))

(defn textured-blob [layers position scale stds color]
  (let [g1 (q/create-graphics (q/width) (q/height))
        g2 (q/create-graphics (q/width) (q/height))]
    (dotimes [_ layers]
      (q/with-graphics g1
        (q/clear)
        (q/no-stroke)
        (q/fill 0)
        (draw-shape (blob position scale stds 0.7 4)))
      (q/with-graphics g2
        (q/clear)
        ;(q/color-mode :hsb 359 100 100 1.0)
        (q/color-mode :rgb 255 255 255 1.0)
        (q/no-stroke)
        (apply q/fill (conj color 0.04))
        (dotimes [_ 100]
          (q/ellipse (rand-int (q/width)) (rand-int (q/height)) 70 90)))
      (mask-w-alpha g2 g1)
      (q/image g2 0 0))))

(defn textured-shape [layers color shape-fn]
  (let [g1 (q/create-graphics (q/width) (q/height))
        g2 (q/create-graphics (q/width) (q/height))]
    (dotimes [_ layers]
      (q/with-graphics g1
        (q/clear)
        (q/no-stroke)
        (q/fill 0)
        (draw-shape (shape-fn)))
      (q/with-graphics g2
        (q/clear)
        ;(q/color-mode :hsb 359 100 100 1.0)
        (q/color-mode :rgb 255 255 255 1.0)
        (q/no-stroke)
        (apply q/fill (conj color 0.04))
        (dotimes [_ 100]
          (q/ellipse (rand-int (q/width)) (rand-int (q/height)) 70 90)))
      (mask-w-alpha g2 g1)
      (q/image g2 0 0))))

(declare wcsketch)
(q/defsketch wcsketch
  :title "Watercolor"
  :settings #(q/smooth)
  :setup setup
  :size [800 800]
  :draw #(do
           (q/stroke-weight 4)
           (q/background 118 136 128)
           (dotimes [_ 70]
             (let [x (rand-int (q/width)) y (rand-int (q/height))]
               ;(q/stroke 30 52 56)
               ;(q/line x y (* 2 x) (* 2 y))
               (q/no-stroke)
               (textured-blob 62 [x y]
                              (+ 15 (gauss 0 5)) [4 3 4 3 9 4 3 4]
                              (rand-nth [[143 93 27] [76 65 29] [30 52 56] [187 149 166]]))))
           (textured-blob 82 [0 400] 800 [40 50 30 20 50 20 15 10]
                          [178 84 87]))
  ;:draw tyler-draw
  ;:draw alpha-testing
  ;:draw #(draw [400 400] [255 0 0])
  :renderer :java2d)

(def pal-bd {:dress-green [137 163 151]
             :pink0 [177 153 168] :pink1 [166 123 147] :pink2 [109 62 89]
             :leave-green [77 72 47] :shirt-red [186 86 86]})
(def trios-sanzo (->> (slurp "resources/sanzo-colors.edn") read-string
                      :combos (filter #(= (count (:id_colors %)) 3))))
(defn genfill-draw []
  (q/background 250)
  (let [layout (rand-nth [:left-single]); :top-single :right-single :bottom-single])
        colors (shuffle (mapv hex-to-rgb (:hex (rand-nth trios-sanzo))))
        third-w (/ (q/width) 3) tw-third-w (* 2 third-w)
        third-h (/ (q/width) 3) tw-third-h (* 2 third-h)
        half-w (/ (q/width) 2)  half-h (/ (q/height) 2)]
    (case layout
      :left-single
      (do 
        (textured-shape
         45 (colors 0)
         #(->> [[-50 -50] [third-w -50]
                [third-w 100] [third-w 200] [third-w 300] [third-w 400]
                [third-w 500] [third-w 600] [third-w 700] [third-w (+ (q/height) 50)]
                [-50 (+ (q/height) 50)]]
               (util.transform/mutate-path 4 0.6 [40 30 20])
               (draw-shape)))
        (textured-shape
         45 (colors 1)
         #(->> [[ third-w -50] [(+ (q/width) 50) -50]
                [(+ (q/width) 50) half-h] [third-w  half-h]
                [ third-w -50]] 
               (util.transform/mutate-path 4 0.6 [10 20 10])
               (draw-shape)))
        (textured-shape
         45 (colors 2)
         #(->> [[third-w half-h] [(+ (q/width) 50) half-h]
                [(+ (q/width) 50) (+ (q/height) 50)] [third-w (+ (q/height) 50)]
                [third-w half-h]] 
               (util.transform/mutate-path 4 0.6 [10 20 10])
               (draw-shape)))
        (textured-shape
         45 (colors 0)
         #(->> [[-50 -50] [third-w -50]
                [third-w 100] [third-w 200] [third-w 300] [third-w 400]
                [third-w 500] [third-w 600] [third-w 700] [third-w (+ (q/height) 50)]
                [-50 (+ (q/height) 50)]]
               (util.transform/mutate-path 4 0.6 [40 30 20])
               (draw-shape)))
        (textured-shape
         45 (colors 1)
         #(->> [[ third-w -50] [(+ (q/width) 50) -50]
                [(+ (q/width) 50) half-h] [third-w  half-h]
                [ third-w -50]] 
               (util.transform/mutate-path 4 0.6 [10 20 10])
               (draw-shape)))
        (textured-shape
         45 (colors 2)
         #(->> [[third-w half-h] [(+ (q/width) 50) half-h]
                [(+ (q/width) 50) (+ (q/height) 50)] [third-w (+ (q/height) 50)]
                [third-w half-h]] 
               (util.transform/mutate-path 4 0.6 [10 20 10])
               (draw-shape)))))))

(declare genfill)
(q/defsketch genfill
  :setup setup
  :size [800 800]
  :draw genfill-draw)
(comment
  (quil.applet/with-applet sketch.watercolor/genfill
    (q/save (str "out/watercolor/bd/" (subs (str (random-uuid)) 0 5) ".png")))) ;
(comment
  (q/sketch
   :setup setup
   :size [800 800]
   :draw (fn []
            ;(q/blend-mode :screen)
           (q/background 118 136 120 0)
           (q/no-stroke)
            ;(q/fill 178 84 87)
            ;(textured-shape 70 [178 84 87]
            ;  (fn []
           (textured-shape 90 [118 136 128]
                           #(->> [[810 -10] [200 -10]
                                  [200 100] [200 200] [200 300] [200 400] [200 500] [200 600] [200 700]
                                  [200 810] [810 810] [810 -10]]
                                 (util.transform/mutate-path 4 0.6 [40 30 20])
                                 (draw-shape)))
           (textured-shape 60 [178 84 87]
                           #(->> [[-10 -10] [200 -10]
                                  [200 100] [200 200] [200 300] [200 400] [200 500] [200 600] [200 700]
                                  [200 810] [-10 810] [-10 -10]]
                                 (util.transform/mutate-path 4 0.6 [40 30 20])
                                 (draw-shape)))
            ;(q/push-matrix)
            ;(q/translate -400 -400)
            ;(q/rotate (q/radians 45))
           (dotimes [_ 20]
             (let [x (+ 200 (rand-int (- (q/width) 200))) y (rand-int (q/height))
                   color (rand-nth [[143 93 27] [76 65 29] [30 52 56] [187 149 166] [187 149 166]])]
               (textured-shape 20 color
                               #(do
                                  (->> (shapes.polygons/hourglass 10 20 3)
                                       (shapes.polygons/close-path)
                                       (util.transform/transform-points 1 [x y])
                                       (util.transform/mutate-path 4 0.6 [2 1 1])
                                       (draw-shape))))))))
           ;(q/translate 400 400))
           ;(q/pop-matrix))

  (declare bdsketch)
  (q/defsketch bdsketch
    :setup setup
    :size [800 800]
    :draw (fn []
            ;(q/blend-mode :screen)
            (q/background 118 136 120 0)
            (q/no-stroke)
            ;(q/fill 178 84 87)
            ;(textured-shape 70 [178 84 87]
            ;  (fn []
            (textured-shape 80 (:shirt-red pal-bd)
                            #(->> [[-50 -50] [200 -50]
                                   [200 100] [200 200] [200 300] [200 400] [200 500] [200 600] [200 700]
                                   [200 850] [-10 850] [-10 -10]]
                                  (util.transform/mutate-path 4 0.6 [40 30 20])
                                  (draw-shape)))
            (textured-shape 90 (:dress-green pal-bd)
                            #(->> [[850 -50] [200 -50]
                                   [200 100] [200 200] [200 300] [200 400] [200 500] [200 600] [200 700]
                                   [200 850] [850 850] [850 -50]]
                                  (util.transform/mutate-path 4 0.6 [40 30 20])
                                  (draw-shape)))
            ;(q/push-matrix)
            ;(q/translate -400 -400)
            ;(q/rotate (q/radians 45))
            (dotimes [_ 20]
              (let [x (+ 200 (rand-int (- (q/width) 200))) y (rand-int (q/height))
                    color (rand-nth (vals (select-keys pal-bd [:pink0 :pink1 :pink2])))]
                (textured-shape 30 color
                                #(do
                                   (->> (poly/hourglass 10 20 3)
                                        (poly/close-path)
                                        (util.transform/transform-points 1 [x y])
                                        (util.transform/mutate-path 4 0.6 [2 1 1])
                                        (draw-shape))))))))
  (quil.applet/with-applet sketch.watercolor/bdsketch
    (q/save (str "out/watercolorzzzz/bd/" (subs (str (random-uuid)) 0 5) ".png"))) ;
  (quil.applet/with-applet sketch.watercolor/wcsketch
    (->> (q/state :l1)
         (q/get-pixel))))
              ;(q/pixels)
              ;(map #(bit-shift-right % 24))
              ;(map #(bit-and % 0xff))
              ;(to-array))))
              ;(q/save (str "out/watercolor_" (subs (str (random-uuid)) 0 5) ".png")))) ;

