(ns dev.hello-triangle
  (:require [clojure.math :as math]
            [quil.core :as q]
            [shapes.polygons :as poly]))

(comment 
  (partition 2 (poly/n-gon 3))
  (math/cos (/ math/PI 3)) ;; 0.5000000000000001
  (math/sin (/ math/PI 3)) ;; 0.8660254037844386
  (math/cos (* 2 (/ math/PI 3))) ;; 2.0943951023931953
  (math/sin (* 2 (/ math/PI 3)))) ;; 0.8660254037844387
  
;y =  (* 500/866 x) + 100
(defn draw [step] 
  (loop [iter 1 x-ini 100 y-ini 100]
    (when (< iter 20)
      (let [x' (+ x-ini step)
            y' (+ (/ 18300 433) (* (/ 500 866) x'))]
        (q/line x-ini y-ini x' y')
        (recur (inc iter) x' y'))))
  (loop [iter 1
         x-ini (+ (* 20 step) 100)
         y-ini (+ (/ 18300 433) (* (/ 500 866) x-ini))]
    (when (< iter 20)
      (let [x' (+ x-ini step)
            y' (+ (/ 18300 433) (* (/ 2094 866) x'))]
        (q/line x-ini y-ini x' y')
        (recur (inc iter) x' y')))))
      
(defn drw []
  (q/background 250)
  (q/push-matrix)
  ;(q/scale 100)
  ;(q/translate [250 (/ (q/sqrt (+ (q/sq 300) (q/sq (/ 300 2)))) 2)])
  (q/translate 400 400)
  (q/rotate (* q/TWO-PI (/ (mod (q/frame-count) 50) 50)))
  #_(let [p1 (->> [0.0 0.0] 
                  (mapv #(* 300 %)))
                 ;(mapv + [320 350]))
          p2 (->> [1.0 0.0]
                  (mapv #(* 300 %)))
                  ;(mapv + [320 350] ))
          p3 (->> (vector (q/cos q/THIRD-PI) (q/sin q/THIRD-PI))
                  (mapv #(* 300 %)))]
                  ;(mapv + [320 350] ))]
      (q/line p1 p2)
      (q/line p2 p3)
      (q/line p3 p1))
  (let [[p1 p2 p3] (partition 2 (poly/n-gon 300 3))]
    (q/line p1 p2)
    (q/line p2 p3)
    (q/line p3 p1))
  (q/pop-matrix))

(defn hello []
  (q/color-mode :rgb 1.0 1.0 1.0 1.0)
  (q/push-matrix)
  (q/rotate q/PI)
  (let [[p1 p2 p3 :as T] (map vec (map #(map + [400 400] %) (partition 2 (poly/n-gon 300 3))))]
    (doseq [x (range 0 (q/width)) y (range 0 (q/height))]
      (let [[dp1 dp2 dp3 :as ds] (map #(q/dist x y (get % 0) (get % 1)) T)
            [w1 w2 w3 :as ws] (map (fn [d] (/ (- 300 d) 300)) ds)
            tds (reduce + ds)
            dns (map #(/ % tds) ds)]
        (if (or (> dp1 (+ dp2 dp3))
                (> dp2 (+ dp1 dp3))
                (> dp3 (+ dp1 dp2)) (some #(> % 300) ds))
            (q/set-pixel x y (q/color 0))
            (q/set-pixel x y (apply q/color ws))))))
  (q/pop-matrix))

(declare sketch)
(q/defsketch sketch
  :setup (fn [] (q/frame-rate 10) (q/no-loop))
  :size [800 800]
  :draw hello)

(quil.applet/with-applet sketch
  (mapv #(* 100 %) (vector (q/cos q/THIRD-PI) (q/sin q/THIRD-PI))))
