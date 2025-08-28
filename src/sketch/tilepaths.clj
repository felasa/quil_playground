(ns sketch.tilepaths
  (:require [quil.core :as q]
            ;[util.transform :refer [transform-shape]]
            [quil.applet]))

(declare tilepaths)
(def combos (->> "resources/sanzo-colors.edn"
                 slurp read-string
                 :combos
                 (filter #(= 4 (count (:id_colors %))))
                 (map :hsb)))
(defn setup []
  ;(q/no-loop)
  (q/frame-rate 3)
  (q/set-state! :palette (shuffle (rand-nth combos))))

(defn arc-tile [cx cy step mode]
  (q/ellipse-mode :center)
  (case mode
    :wn-es
    (do (q/arc cx cy step step 0 (* 1 q/HALF-PI))
        (q/arc (+ cx step) (+ cy step) step step q/PI (* 3 q/HALF-PI)))
    :ne-sw
    (do (q/arc (+ cx step) cy step step q/HALF-PI q/PI)
        (q/arc cx (+ cy step) step step (* 3 q/HALF-PI) q/TWO-PI))))

(defn line-tile [cx cy step mode]
  (q/stroke-cap :project)
  (case mode
    :wn-es
    (do 
      (q/line [cx  (+ cy (/ step 2))]
              [(+ cx (/ step 2)) cy])
      (q/line [(+ cx step) (+ cy (/ step 2))]
              [(+ cx (/ step 2)) (+ cy step)]))
    :ne-sw
    (do 
      (q/line [(+ cx (/ step 2)) cy]
              [(+ cx step) (+ cy (/ step 2))])
      (q/line [(+ cx (/ step 2)) (+ cy step)]
              [cx  (+ cy (/ step 2))]))))

(def otile (partial arc-tile 0 0))

(defn plaster [step palette]
  (q/stroke-weight 6) ;param increase/decrease or make random in doseq
  (q/stroke-cap :project)
  (q/no-fill)
  (q/color-mode :hsb 359 100 100 1.0)
  (apply q/background (palette 0))
  (doseq [x (range 0 (q/width) step)
          y (range 0 (q/height) step)]
    (apply q/stroke (rand-nth (subvec palette 1))) ;param constant or random
    #_(if (rand-nth [true false]) 
        (apply q/fill (rand-nth (subvec palette 1)))
        (q/no-fill)) ;param turn on/off make random
    (q/push-matrix)
    (q/translate x y)
    ((rand-nth [arc-tile]) 0 0 step (rand-nth [:wn-es :ne-sw])) ;param choose arc or straigh or random
    (q/pop-matrix)))
  ;(println (q/state)))
  ;(q/save (str "out/tilepaths/tilepath_" (subs (str (random-uuid)) 0 5) ".png"))) ;

(def pal (shuffle (rand-nth combos)))
(q/defsketch tilepaths
  ;:draw #(do (tile 400 400 200 :ne-sw) (tile 600 400 200 :wn-es))
  :draw #(plaster (/ 800 20) (q/state :palette))
  ;;:draw #(plaster (/ 800 10) [[23 86 96] [219 14 8] [160 100 58] [16 41 93]])
  :settings #(q/smooth)
  :setup setup
  :size [800 800])

(quil.applet/with-applet sketch.tilepaths/tilepaths
  (q/save (str "out/tilepaths/tilepath_" (subs (str (random-uuid)) 0 5) ".png"))) ;
