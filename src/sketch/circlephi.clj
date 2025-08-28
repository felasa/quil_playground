;; taken from http://quil.info/sketches/show/example_golden-ratio-flower
(ns sketch.circlephi
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def sanzo-pals (->> "resources/sanzo-colors.edn" 
                     slurp
                     read-string
                     :combos
                     (mapv :hsb)))
(declare grf)
(declare grf-fn)
(def PHI (/ (+ 1 (Math/sqrt 5)) 2))

(def palette
  (cycle [[249 187  78]
          [ 70 162 141]
          [220 112 100]]))

(defn setup []
  (q/no-loop)
  (q/set-state! :palette (cycle (rand-nth sanzo-pals))))

(defn setup-fn-mode []
  (q/background 255 255 236)
  (q/frame-rate 5)
  ;(q/set-state! :palette (cycle (rand-nth sanzo-pals)))
  {:i 0 :palette (cycle (rand-nth sanzo-pals))})

(defn update-sketch [state]
  ; increase radius of the circle by 1 on each frame
  (update-in state [:i] inc))

(defn draw []
  (q/ellipse-mode :radius)
  (q/no-stroke)
  (q/background 255 255 236)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (doseq [i (range 1000)]
      (let [v (+ (mod (q/frame-count) 3) i)
            ang (* v PHI q/TWO-PI) ;angle increases in steps of phi
            r   (* (Math/sqrt v) (q/width) (/ 70))
            x   (* (q/cos ang) r)
            y   (* (q/sin ang) r)
            sz  (+ 3 (* i 0.005))]
        (apply q/fill (nth (q/state :palette) i))
        (q/ellipse x y sz sz)))))

(defn draw-fn [state]
  (q/ellipse-mode :radius)
  (q/no-stroke)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (let [i (:i state)
          v (+ (mod (q/frame-count) 3) i)
          ang (* v PHI q/TWO-PI) ;angle increases in steps of phi
          r   (* (Math/sqrt v) (q/width) (/ 70))
          x   (* (q/cos ang) r)
          y   (* (q/sin ang) r)
          sz  (+ 3 (* i 0.005))]
      (apply q/fill (nth palette i))
      (q/ellipse x y sz sz))))

(q/defsketch grf
  ;:host "host"
  :size [800 800]
  :setup setup
  :draw draw)

(q/defsketch grf-fn
  ;:host "host"
  :size [800 800]
  :setup setup-fn-mode
  :draw draw-fn
  :update update-sketch
  :middleware [m/fun-mode])
