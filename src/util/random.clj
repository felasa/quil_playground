(ns util.random
  (:require [quil.core :as q]
            [clojure.math :as math]))

(defn gauss
  [mu sigma]
  (+ mu (* sigma (q/random-gaussian))))

;; 'manual' simulations, probably better sticking with library ones
;; but it would be desirable to get methods from outside of processing lib
(defn draw-binormal 
  ([]
   (let [u1 (rand)
         u2 (rand)]
     [(* (math/sqrt (* -2 (math/log u1))) (math/cos (* 2 math/PI u2)))
      (* (math/sqrt (* -2 (math/log u1))) (math/sin (* 2 math/PI u2)))]))
  ([[xm ym :as mean]] 
   (let [Z (draw-binormal)]
     (mapv + mean Z)))
  ([mean [stdx stdy :as std]]
   (let [Z (draw-binormal)]
     (mapv + mean (map * std Z)))))

(defn rbinorm
  [times & params]
  (let [sample (transient (vector))]
    (dotimes [n times]
      (conj! sample (apply draw-binormal params)))
    (persistent! sample)))

(defn rnorm
  ([times]
   (let [sample (transient (vector))]
     (dotimes [n times]
       (conj! sample ((draw-binormal) 0)))
     (persistent! sample)))
  ([times mean std] (mapv #(+ mean %) (map #(* std %) (rnorm times)))))

