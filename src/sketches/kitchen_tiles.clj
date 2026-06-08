(ns sketches.kitchen-tiles
    (:require [quil.core :as q]
              [util.core :refer [hex-to-rgb]]))

;; TODO: Tidy up and make clear how to control the variations on holes (missing tiles),
;; rotation and rgb shift

(declare sketch)
(def sanzo (read-string (slurp "resources/sanzo-colors.edn")))
(def palette (map hex-to-rgb (:hex (rand-nth (:combos sanzo)))))

(defn draw []
  (q/background 50)
  (q/no-stroke)
  (let [fraction 30
        padding 2
        sqw (/ (q/width) fraction) sqh sqw]
    (doseq [x (range 0 (q/width) sqw)
            y (range 0 (q/height) sqh)]
      (if (< (rand) 0.33) (q/fill 255 255 0)
        (if (< (rand) 0.66) (q/fill 0 255 255) 
          (q/fill 255 0 255)))
      (when
        (< (* 1.5 (rand)) (/ y (q/height))) 
        (q/rect (+ x padding) (+ y padding) (- sqw (* 2 padding)) (- sqh (* 2 padding)))))))

(defn draw2 []
  (q/background 50)
  (q/no-stroke)
  (let [fraction 30
        padding 2
        sqw (/ (q/width) fraction) sqh sqw]
    (doseq [x (range 0 (q/width) sqw)
            y (range 0 (q/height) sqh)]
      (if (< (rand) 0.33) (q/fill 255 255 0)
        (if (< (rand) 0.66) (q/fill 0 255 255) 
          (q/fill 255 0 255)))
      (q/push-matrix)
      (q/translate (+ x padding) (+ y padding))
      (q/rotate (* (rand q/QUARTER-PI) (- 1 (/ y (q/height)))))
      (q/rect 0 0 (- sqw (* 2 padding)) (- sqh (* 2 padding)))
      (q/pop-matrix))))

(defn rand-offset [[x y]]
  ;(* 2 (q/sq (+ 1 (- 1 (/ y (q/height))))) (q/random-gaussian))
  (* (- (rand-int 3) 1) (q/sq (+ 1 (- 1 (/ y (q/height)))))))

(defn rand-offset-fn
  "return discrete fn"
  [[fraction margin padding]] 
  (let [dic (apply merge (map-indexed (fn [idx v] {v idx}) (reverse (range margin (- 800 margin) (/ (- 800 (* 2 margin)) fraction)))))]
    (fn [y] (* (get dic y) (q/random-gaussian))))) 


(defn draw3 []
  (q/blend-mode :add)
  (q/background 50)
  (q/no-stroke)
  (let [pal (map hex-to-rgb (:hex (rand-nth (:combos sanzo))))
        margin 10 fraction 10
        padding 5
        sqw (/ (- (q/width) (* 2 margin)) fraction)
        sqh (/ (- (q/height) (* 2 margin)) fraction)
        colors (atom {})
        rand-offset (rand-offset-fn [fraction margin padding])
        ;rotations (atom {}) 
        ;rand-color (rand) rand-rotate (rand q/QUARTER-PI)
        ;red   (cond (< rand-color 0.33) 255 (< rand-color 0.66) 0   :else 255)
        ;green (cond (< rand-color 0.33) 255 (< rand-color 0.66) 255 :else 0)
        ;blue  (cond (< rand-color 0.33) 0   (< rand-color 0.66) 255 :else 255)
        alpha 128]
        ;r-layer (q/create-graphics (q/width) (q/height))
        ;g-layer (q/create-graphics (q/width) (q/height))
        ;b-layer (q/create-graphics (q/width) (q/height))]
    (doseq [x (range margin (- (q/width) margin) sqw)
            y (range margin (- (q/height) margin) sqh)]
      (swap! colors (fn [kv] (assoc-in kv [[x y] :color] (rand-nth pal))))
      (swap! colors (fn [kv] (assoc-in kv [[x y] :rotation] (* 0.9 (rand q/QUARTER-PI)))))
      (let [color (get-in @colors [[x y] :color])]
            ;red (cond (< (get-in @colors [[x y] :color]) 0.33) 128 (< (get-in @colors [[x y] :color]) 0.66) 0 :else 255)]
        ;(q/fill red 100 red alpha)
        (q/fill (color 0) (color 1) 0 alpha)
        (q/push-matrix)
        (q/translate (+ x padding (rand-offset  y))
                     (+ y padding (rand-offset  y)))
        (q/rotate (* (get-in @colors [[x y] :rotation]) (- 1 (/ y (q/height)))))
        (when true (q/rect (+ 0 ) (+ 0 ) (- sqw (* 2 padding)) (- sqh (* 2 padding))))
        (q/pop-matrix)))
    (doseq [x (range margin (- (q/width) margin) sqw)
            y (range margin (- (q/height) margin) sqh)]
      (let [color (get-in @colors [[x y] :color])]
            ;green (cond (< (get-in @colors [[x y] :color]) 0.33) 255 (< (get-in @colors [[x y] :color]) 0.66) 255 :else 0)]
         ;(q/fill green green 32 alpha)
         (q/fill 0 (color 1) (color 2) alpha)
         (q/push-matrix)
         (q/translate (+ x padding (rand-offset  y))
                      (+ y padding (rand-offset  y)))
         (q/rotate (* (get-in @colors [[x y] :rotation]) (- 1 (/ y (q/height)))))
         (when true (q/rect 0 0 (- sqw (* 2 padding)) (- sqh (* 2 padding))))
         (q/pop-matrix)))
    (doseq [x (range margin (- (q/width) margin) sqw)
            y (range margin (- (q/height) margin) sqh)]
      (let [color (get-in @colors [[x y] :color])]
            ;blue (cond (< (get-in @colors [[x y] :color]) 0.33) 0 (< (get-in @colors [[x y] :color]) 0.66) 255 :else 255)]
         ;(q/fill 32 blue blue alpha)
         (q/fill (color 0) 0 (color 2) alpha)
         (q/push-matrix)
         (q/translate (+ x padding (rand-offset  y))
                      (+ y padding (rand-offset  y)))
         (q/rotate (* (get-in @colors [[x y] :rotation]) (- 1 (/ y (q/height)))))
         (when true (q/rect 0 0 (- sqw (* 2 padding)) (- sqh (* 2 padding))))
         (q/pop-matrix)))))
    ;(q/blend g-layer r-layer 0 0 800 800 0 0 800 800)
    ;(q/blend g-layer 0 0 800 800 0 0 800 800 :add)

(defn kitchen-tiles []
  (q/sketch 
    :size [800 800]
    :setup (fn [] (q/no-loop))
    :draw draw3
    :drawa draw2
    :drawc (fn []
             (let [g (q/create-graphics 800 800)]
               (q/with-graphics g
                 ;(q/background 255 255 255 0)
                 (q/blend-mode :subtract)
                 (q/no-stroke)
                 (q/fill 0 0 255)
                 (q/ellipse 200 200 400 400)
                 (q/fill 255 0 0)
                 (q/ellipse 400 200 400 400)
                 (q/fill 0 255 0)
                 (q/ellipse 200 400 400 400))
               (q/image g 0 0)))
    :drawc (fn [] 
             (q/rect-mode :center)
             (q/push-matrix)
             (q/translate 400 400)
             (q/rotate (/ q/QUARTER-PI 2)) 
             (q/rect 0 0 400 400)
             (q/pop-matrix))
    :drawa (fn []
             (q/background 0)
             (q/blend-mode :add)
             (q/fill 45 78 128)
             (q/rect 100 100 300 300)
             (q/fill 45 0 0)
             (q/rect 110 450 300 300)
             (q/fill 0 78 0)
             (q/rect 100 460 300 300)
             (q/fill 0 0 128)
            (q/rect 90 440 300 300))))

(comment 
  (kitchen-tiles)
  (quil.applet/with-applet dev.grid/sketch 
    (q/save (str "out/grid_tiles/deterioro_" (subs (str (random-uuid)) 0 5) ".png")))) ;
