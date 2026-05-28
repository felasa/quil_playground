;; fractal tree algo stolen from x.com/i/status/2057186570210275383
(ns fractal-tree
  (:require [quil.core :as q]
            [util.masking :as msk]
            [util.color :as color]))

(def palette {:jasper-red "#eb5324"
              :olympic-blue "#5b82b4"
              :light-mauve "#9a72aa"})

(defn setup [] 
  (q/no-loop))

(defn branch [stroke-weight len sym?]
  (q/stroke-cap :project)
  ;(q/no-stroke)
  (q/stroke-weight stroke-weight)
  (q/line 0 0 0 (- len))
  (q/with-translation [0  (- len)]
    (if (> len 2)
      (do
        (q/push-matrix)
        (if sym? (q/rotate (/ q/PI 6))    ;symetrical
         (q/rotate (+ (/ q/PI 2.5) (* 0.55 (rand) (q/random-gaussian))))) ;titrate
        (branch (max (* stroke-weight 0.618) 1) (* len 0.66) sym?) ;OG 0.67
        (q/pop-matrix)
        (q/push-matrix)
        (q/rotate (+ (/ q/PI 6 -1) (* 0.55 (rand) (q/random-gaussian))))    ;symetrical
        (branch (max (* stroke-weight 0.618) 1) (* len 0.66) sym?)
        (q/pop-matrix))
      (when (> (rand) 0.5)
        (let [color (rand-nth (map color/hex-to-rgb (vals palette)))]
          (q/with-stroke nil
            (q/with-fill color
               (q/ellipse 0 0 (max (* 5 stroke-weight 0.618) 1) (max (*  5 stroke-weight 0.618) 1)))))))))

;cinnamon buff for background HSB: 34, 51, 99. RGB 253, 197, 125
(defn draw [len sym]
  (q/color-mode :rgb)
  (q/background 40)
  ;(q/background 253 197 125)
  (let [smoke (q/create-graphics 750 750)
        tree  (q/create-graphics 750 750)
        bg    (q/create-graphics 750 750)]
    (q/with-graphics bg
      (let [W (.width bg) H (.height bg)]
        (q/color-mode :hsb 359 100 100 1.0)
        (q/background 34 51 99)
        (q/color-mode :rgb 255 255 255)
        (doseq [x (range 750) y (range 750)]
          (let [gx (/ (abs (- x (/ W 2))) (/ H 2))
                gy (/ (abs (- y (/ W 2))) (/ H 2))
                dval (- (Math/pow (+ (max gx gy) 0.0) 4) 0.4) 
                [sx sy] (mapv #(* % 4.537) [x y])
                noiseval (q/noise sx sy)]
            (when (> dval noiseval) (q/set-pixel x y (q/color 40)))))))
    (q/with-graphics smoke
      (q/clear)
      (q/no-stroke)
      (dotimes [_ 1800]
        (q/fill (+ 200 (rand-int 55)) 4)
        (q/ellipse (rand-int (q/width)) (rand-int (q/height)) 50 50)))
    (q/with-graphics tree
      (let [W (.width tree) H (.width  tree)]
        (q/clear)
        ;(q/background 240)
           ;; [253 197 125]
        ;(q/stroke 255)
        (q/stroke 40)
        ;(q/stroke 40 20 10)
        (q/with-translation [(/ W 2) (/ (+ H len) 2)]
          (branch 20 len sym))
        (q/with-translation [(/ W 2) (+ (/ (- len) 2) (/ H 2))]
          (q/scale -1 -1)
          (branch 20 len sym))))
        ;;add sym side trees
    (q/image bg 25 25)
    ;(msk/mask-w-alpha tree smoke)
    (q/image tree 25 25)
    #_(doseq [x (range (q/width)) y (range (q/height))]
       (let [gx (/ (abs (- x (/ (q/width) 2))) (q/width))
             gy (/ (abs (- y (/ (q/height) 2))) (q/height))
             dval (max gx gy)
             scaled-x (* x 2.13) scaled-y (* y 2.13)
             noiseval (q/noise scaled-x scaled-y)]
         (when (> (* 0.5 dval) noiseval)
           (q/set-pixel x y (q/color 255)))))))


(declare sketch)
(q/defsketch sketch
  :size [800 800]
  :settings #(q/smooth 16)
  :setup setup
  :draw #(draw 150 false))
(comment 
  (quil.applet/with-applet sketch
    (q/save (str "out/fracta_trees_" (subs (str (random-uuid)) 0 5) ".png")))) ;
