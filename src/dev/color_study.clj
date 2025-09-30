(ns dev.color-study
  (:require [quil.core :as q]))
  
(declare sketch)
;; rough game plan
;;  define cluster zones
;;  define value ranges within cluster 
;;  ???
;;  profit
(defn stettings [H]
  (let [n-clusters (rand-nth [2 3 4])
        cluster-hs (take n-clusters (shuffle (range H)))
        cluster-thick (for [i n-clusters] (rand-nth [10 20 30 40 50 60 70]))]))
         
        
(defn draw [hue]
  (q/color-mode :hsb 359 100 100 1.0)
  (q/background hue 50 50)
  (q/stroke-weight 3)
  (dotimes [y (- (q/height) 80)]
    (q/stroke
      hue
      ;(+ 50 (- (+ (rand-int (quot y 40)) 2) (quot y 80) 1))
      ;(+ 50 (- (+ (rand-int (quot y 40)) 2) (quot y 80) 1)))
      (+ 50 (* 3 (q/random-gaussian)))
      (+ 50 (* 3 (q/random-gaussian))))
    (q/line 40 (+ y 40) (- (q/width) 40) (+ y 40))))
    
(q/defsketch sketch
  :size [800 800]
  :setup (fn [] (q/no-loop))
  :draw #(draw 240))


