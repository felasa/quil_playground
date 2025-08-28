(ns dev.mandelbrot
  (:require [quil.core :as q]
            [quil.applet]
            [clojure.math :as math]))


(defn z-square [[^float x ^float y]]
  ;(x + yi)(x + yi) = x*x + 2x*yi - y*y
  (vector (- (* x x) (* y y)) (* 2 x y)))

(defn norm-sq [[^float x ^float y]]
  (+ (* x x) (* y y)))

(defn z-add [z z']
  (mapv + z z'))

(defn mandel-fn [c z]
  (z-add (z-square z) c))

(defn is-mandel?
  [iters distance c z]
  (let [p (nth (iterate (partial mandel-fn c) z) iters)
        N (norm-sq p)]
    (< N distance)))

(defn new-is?
  [iters distance c z]
  (loop [iter 1 zn z]
    (if (> iter iters) true
      (if (> (norm-sq zn) distance) false 
        (recur (inc iter) (mandel-fn c zn))))))

(defn transform-coord 
  ([[x y]] 
   (vector (* 1.0  (/ (- (* 2 x) (q/width))  (q/width)))
           (* -1.0 (/ (- (* 2 y) (q/height)) (q/height)))))
  ([scale [x y]] (mapv #(* scale %) (transform-coord [x y])))) 

(defn draw-julia
  [iters distance c]
  (doseq [x (range (q/width))
          y (range (q/height))]
    (when (new-is? iters distance c (transform-coord 1 [x y]))
      (q/set-pixel x y (q/color 0)))))
  ;(println "done"))

(defn draw-mandel
  [iters distance]
  (q/color-mode :rgb 1.0 1.0 1.0 1.0)
  (doseq [x (range (q/width))
          y (range (q/height))]
    (when (new-is? iters distance (transform-coord 1 [x y]) [0 0])
      (q/set-pixel x y (q/color (/ x (q/width)) (/ y (q/height)) (* (/ x (q/width)) (/ y (q/height)))))))
  (println "done"))

(declare fractal)
(q/defsketch fractal
  :setup (fn [] (q/no-loop) (q/background 255))
  :size [800 800]
  :drawa #(draw-julia 20 2 [-0.5125 -0.5213])
  :draw #(draw-mandel 100 5))

(quil.applet/with-applet fractal
  (transform-coord [800 800]))
