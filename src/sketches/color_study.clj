(ns sketches.color-study
  (:require [quil.core :as q]))
  
(declare sketch)
;; rough game plan
;;  define cluster zones
;;  define value ranges within cluster 
;;  ???
;;  profit

;; remove this and other draw fns?
(defn stettings [H]
  (let [n-clusters (rand-nth [3 4 5 6])
        cluster-hs (take n-clusters (shuffle (range 40 (- H 40))))
        cluster-thicks (for [i (range n-clusters)] (+ 40 (rand-int 80)))
        sats (for [i (range n-clusters)]
               (rand-nth (range 40 61)))
        blacks (for [i (range n-clusters)]
                 (rand-nth (range 49 51)))]
    {:hs (vec cluster-hs) 
     :thicks (vec cluster-thicks)
     :sats (vec sats)
     :blacks (vec blacks)}))
         

(comment 
  (stettings 800))

(defn draw2 [hue]
  (q/color-mode :hsb 359 100 100 1.0)
  (q/background hue 50 50)
  (q/stroke-weight 3)
  (let [stets (stettings (q/width))
        n-clusters (count (:hs stets))]
    (dotimes [c n-clusters]
      (let [yinf (- (get (:hs stets) c) (get (:thicks stets) c))
            ysup (+ (get (:hs stets) c) (get (:thicks stets) c))
            base-sat (get (:sats stets) c)
            base-black (get (:blacks stets) c)]
        (doseq [y (range yinf ysup 30)]
          (let [gap-sat (+ base-sat (* 5 (q/random-gaussian)))
                gap-black (+ base-black (* 5 (q/random-gaussian)))]
            (dotimes [i 30]
              (q/stroke (+ hue) gap-sat (+ gap-black)) 
              ;(q/stroke (+ hue (q/random-gaussian)) (+ base-sat (* 2 (q/random-gaussian))) (+ base-black (* 2 (q/random-gaussian))))
              (when (< (+ y i) (- 800 40)) (q/line 40 (+ y i) (- (q/width) 40) (+ y i))))))))))
              
(defn draw3 [hue]
  (q/color-mode :hsb 359 100 100 1.0)
  (q/background hue 50 50)
  (q/stroke-weight 3)
  (q/stroke hue (+ 30 (rand-int 50)) (+ 70 (rand-int 30)))
  (doseq [y (range 40 760)]
    (if (< (rand) 0.05)
      (q/stroke hue (+ 50 (rand-int 50)) (+ 70 (rand-int 30)))
      (when (< (rand) 0.1) (q/stroke hue 50 50)))
    (q/line 40 y (- (q/width) 40) y)))
    

(defn draw [hue]
  (q/color-mode :hsb 359 100 100 1.0)
  (q/background hue 70 50)
  (q/stroke-weight 3)
  (dotimes [y (- (q/height) 80)]
    (q/stroke
      hue
      ;(+ 50 (- (+ (rand-int (quot y 40)) 2) (quot y 80) 1))
      ;(+ 50 (- (+ (rand-int (quot y 40)) 2) (quot y 80) 1)))
      (+ 50 (* 3 (q/random-gaussian)))
      (+ 50 (* 3 (q/random-gaussian))))
    (q/line 40 (+ y 40) (- (q/width) 40) (+ y 40))))
    
(defn draw4 [hue ini-sat ini-b p-cluster-remain p-cluster-start]
  (q/color-mode :hsb 359 100 100 1.0)
  (q/background hue ini-sat ini-b)
  (q/stroke-weight 1)
  (dotimes [_ 1] 
    (loop [y 60 
           sat ini-sat bri ini-b
           cluster? false]
      (when (< y (- (q/height) 60))
          (q/stroke hue sat bri 1.0)
          (if cluster? 
            (do 
              (q/line 60 y (- (q/width) 60) y) 
              (recur (inc y) (+ sat (q/round (* 1 (q/random-gaussian)))) (+ bri (q/round (* 1 (q/random-gaussian)))) 
                     (if (< (rand) p-cluster-remain) true false)))
            (if (< (rand) p-cluster-start)
              (recur y (+ 30 (rand-int 40)) (+ 50 (rand-int 50)) true)
              (recur (inc y) sat bri false)))))))

(q/defsketch sketch
  :size [800 800]
  :setup (fn [] (q/no-loop))
  :draw #(draw4 (rand-int 360) (rand-int 100) (rand-int 100) 0.95 (rand))) 
 
