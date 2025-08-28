;; based on https://www.tylerxhobbs.com/words/a-randomized-approach-to-circle-packing
(ns sketch.circlepacking
  (:require [quil.core :as q]
            [quil.applet]
            [clojure.math :as math]
            [util.color :refer [hex-to-rgb]]
            [clojure.data.json :as json]))

(def combos (filter #(= (count (% "id_colors")) 4) ((json/read-str (slurp "resources/sanzo-colors.json")) "combos")))

;; just use q/dist?
(defn euc
  [x y x' y']
  (math/sqrt (+ (math/pow (- x x') 2) (math/pow (- y y') 2))))

(defn collides? 
  [[x y r] [x' y' r']] 
  (<= (euc x y x' y') (+ r r')))

;; may be more efficient? no need anyway
;; (+ (square (- x x')) (square (- y y'))) <= (square (+ r R))
(defn draw
  [tries ini-r anneal-r]
  (q/ellipse-mode :radius)
  (let [pallete (mapv hex-to-rgb ((rand-nth combos) "hex"))]
    (loop [drawn [] remaining tries R ini-r]
      (cond 
        (<= R 1) nil
        (<= remaining 0) (recur drawn tries (* R anneal-r))
        :else 
        (let [buff (+ R 10) ;; Margin
              x (+ buff (rand-int (- (q/height) (* 2 buff)))) y (+ buff (rand-int (- (q/width) (* 2 buff))))
              color (rand-nth pallete)]
          (if (not-any? (partial collides? [x y R]) drawn)
            (do
              (apply q/stroke (conj color 128))
              (apply q/fill (conj color 128))
              (q/ellipse x y (* R 0.99) (* R 0.99)) ;;factor controls how separated circles are
              (recur (conj drawn [x y  R]) (dec remaining) R)) ;;may be spilling here, look cool tho
            (recur drawn (dec remaining) R)))))))
                    
(defn setup []
  (q/no-loop)
  ;(q/frame-rate 8)
  ;(q/background 50)
  (q/background 200 0)
  (q/stroke-weight 1))

(declare circlepack)
(q/defsketch circlepack
  :title "CPacking"
  :display 1
  :settings #(q/smooth)
  :setup setup
  :draw #(dotimes [_ 1] (draw 20000 100 0.5))
  :size [900 900]
  :features [:resizable]
  :renderer :java2d)

(comment 
  (quil.applet/with-applet sketch.circlepacking/circlepack 
    (q/background 0))
  (quil.applet/with-applet sketch.circlepacking/circlepack 
    (q/save (str "out/circles_packs_" (subs (str (random-uuid)) 0 5) ".png")))) ;
