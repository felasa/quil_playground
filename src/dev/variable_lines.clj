(ns dev.variable-lines
  (:require [quil.core :as q]))

(declare sketch)
(def sanzo-colors (->> (slurp "resources/sanzo-colors.edn")
                       read-string
                       :combos))

(defn draw [steps]
  (let [x-ini (rand-int (q/width)) y-ini (rand-int (q/width))
        angle-ini (rand) weight-ini 10]
    (loop [iter 1 x x-ini y y-ini angle angle-ini weight weight-ini]
      (when (and (<= iter steps) (>= weight 0)) 
        (let [x' (+ x (* 5 (q/cos angle))) y' (+ y (* 5 (q/sin angle)))
              w' (+ weight (* 0.25 (- (q/random-gaussian) 1)))
              a' (+ angle (q/random-gaussian))]
          (q/stroke-weight weight)
          (q/line x y x' y')
          (recur (inc iter) x' y' a' w'))))))


(q/defsketch sketch 
  :setup (fn [] (q/no-loop))
  :size [800 800]
  :draw #(do 
           (q/color-mode :hsb 359 100 100 1.0)
           (let [pal (:hsb (rand-nth sanzo-colors))]
             (dotimes [_ 5000]
               (apply q/stroke (rand-nth pal))
               (draw 1000))))) 
  
