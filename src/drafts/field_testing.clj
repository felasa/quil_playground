(ns drafts.field-testing
  (:require [quil.core :as q]
            [quil.applet :as qapp]))

(declare sketch)
(defn scale-with-margin-fn
  [scale]
  (fn [[x y]] (vector (+ (* x scale) (/ (q/width) 4)) (+ (* y scale) (/ (q/height) 4)))))

(defn perlin-field 
  [p]
  (let [value (apply q/noise p)
        angle (* q/TWO-PI value)]
    (vector (q/cos angle) (q/sin angle))))

(defn curl-field [[x y :as p]]
  (let [delta 1e-2
        noise-fn (fn [p] (apply q/noise p))
        x0 (- x delta) x1 (+ x delta)
        y0 (- y delta) y1 (+ y delta)
        dfx (- (noise-fn [x0 y]) (noise-fn [x1 y]))
        dfy (- (noise-fn [x y0]) (noise-fn [x y1]))
        V [(/ (- dfy) delta) (/ dfx delta)] N (apply q/mag V)]
    (mapv #(/ % N) V)))
            
(defn perlin-path
  [scale num-segments segment-len]
  (fn [[x y :as p]]
    (loop [p0 p iter 1 
           return []]
      (if (<= iter num-segments)
        (let [delta (mapv #(* segment-len %) (perlin-field ((scale-with-margin-fn scale) p0)))
              p1 (mapv + p0 delta)]
          (recur p1 (inc iter) (conj return [p0 p1])))
        return))))
    
(defn field-path
  [field-fn scale num-segments segment-len]
  (fn [[x y :as p]]
    (loop [p0 p iter 1 
           return []]
      (if (<= iter num-segments)
        (let [delta (mapv #(* segment-len %) (field-fn  ((scale-with-margin-fn scale) p0)))
              p1 (mapv + p0 delta)]
          (recur p1 (inc iter) (conj return [p0 p1])))
        return))))

(q/defsketch sketch
  :size [800 800]
  :setup (fn [] (q/no-loop))
  :settings (fn [] (q/smooth))
  :draw (fn []
           (dotimes [_ 500]
              (let [points ((field-path perlin-field 0.0035 1500 2) [(rand-int (q/width)) (rand-int (q/height))])]
               (doseq [[p0 p1] points] (q/line p0 p1)))))
  :drawn (fn []
           (dotimes [_ 500]
             (let [points ((field-path curl-field 0.0035 30 5) [(rand-int (q/width)) (rand-int (q/height))])]
              (doseq [[p0 p1] points] (q/line p0 p1))))))

(declare rays)
[0 1]
[1 2]
(defn between? [x lb up]
  (<= lb x up))

(some #(apply between? 3.2 %) (partition 2 (map #(* % q/PI) (map #(+ % 1) (range 0 1 0.15)))))
;; ((3.1415927410125732 3.612831652164459)
;;  (4.084070563316345 4.555309474468231)
;;  (5.026548385620117 5.497787296772003))

(partition 2 (range 0 1 0.1))
(defn sketch []
  (q/defsketch rays
   :size [800 800]
   :setup (fn [] (q/no-loop))
   :draw  (fn []
            (q/background 0)
            (q/stroke 156 40 78)
            (q/stroke-weight 2)
            (dotimes [_ 900]
              (let [points ((field-path curl-field 0.0035 30 5) [(rand-int (q/width)) (rand-int (q/height))])]
               (doseq [[p0 p1] points] (q/line p0 p1))))
            (let [intervals (partition 2 (map #(* % q/PI) (map #(+ % 1) (range 0 2 0.2))))]
              (doseq [x (range (q/width)) y (range (q/height))]
                 ;;(when (< (rand) (* 0.e (/ y (q/width)))) (q/set-pixel x y 255)) 
               (let [pxl (q/get-pixel x y)
                     [dx dy] (mapv - [(/ (q/width) 2) (q/height)] [x y])
                     angle (+ q/PI (q/atan2 dy dx))]
                 (when (some #(apply between? angle %) intervals)
                   (q/set-pixel x y (bit-xor 0xFFFFFF pxl))
                   (when (< (rand) (* 0.8 (/ (q/sqrt (- (q/width) y)) (q/width)))) (q/set-pixel x y (q/color 255))))))) 
            (q/no-stroke)
            (q/color-mode :hsb 359 100 100 1.0) (q/fill 40 100 100)
            (q/ellipse (/ (q/width) 2) (q/height) 200 200))))

(qapp/with-applet sketch 
  (q/noise-seed (rand))
  (let [noises (for [x (range (q/width)) y (range (q/height))]
                 (q/noise x y))]
    (vector (reduce min noises) (reduce max noises))))
