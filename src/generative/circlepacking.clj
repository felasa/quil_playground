;; based on https://www.tylerxhobbs.com/words/a-randomized-approach-to-circle-packing
(ns generative.circlepacking
  (:require [quil.core :as q]
            [clojure.math :as math]
            [util.core :refer [local-context hex-to-rgb]]
            [clojure.data.json :as json]))

(def params 
  (let [Width 800
        Height 800
        border-buffer 5
        lc (local-context)]
    (update-keys lc keyword)))

(def combos (filter #(= (count (% "id_colors")) 4) ((json/read-str (slurp "resources/sanzo-colors.json")) "combos")))
(defn euc [x y x' y']
  (math/sqrt (+ (math/pow (- x x') 2) (math/pow (- y y') 2))))

(defn collides? 
  [[x y r] [x' y' r']] 
  (<= (euc x y x' y') (+ r r')))

;; may be more efficient? no need anyway
;; (+ (square (- x x')) (square (- y y'))) <= (square (+ r R))
(defn draw [W H tries ini-r anneal-r]
  (let [pallete (mapv hex-to-rgb ((rand-nth combos) "hex"))]
    (loop [drawn [] remaining tries R ini-r]
      (cond 
        (< R 5) nil
        (<= remaining 0) (recur drawn tries (* R anneal-r))
        :else 
        (let [buff (+ R 10)
              x (+ buff (rand-int (- H (* 2 buff)))) y (+ buff (rand-int (- W (* 2 buff))))
              color (rand-nth pallete)]
          (if (not-any? (partial collides? [x y R]) drawn)
            (do
              (apply q/stroke color)
              (apply q/fill color)
              (q/ellipse x y (* R 1.9) (* R 1.9))
              (recur (conj drawn [x y R]) (dec remaining) R))
            (recur drawn (dec remaining) R)))))))
                    
; [--x--][--y--]
(defn setup []
  ;(q/frame-rate 8)
  ;(q/background 50)
  (q/background 200 0)
  (q/stroke-weight 1)
  (q/no-loop))

(comment 
  (q/defsketch example
    :title "CPacking"
    :display 1
    :settings #(q/smooth)
    :setup setup
    :draw #(dotimes [n 1] (draw 900 900 20000 100 0.5))
    :size [900 900]
    :features [:resizable]
    :renderer :java2d))

(comment 
  (quil.applet/with-applet generative.circlepacking/example 
    (q/background 0))
  (quil.applet/with-applet generative.circlepacking/example 
    (q/save (str "out/circles_packs_" (subs (str (random-uuid)) 0 5) ".png")))) ;
