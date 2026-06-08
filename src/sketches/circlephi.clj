;; taken from http://quil.info/sketches/show/example_golden-ratio-flower
(ns sketches.circlephi
  (:require [quil.core :as q]))

(def sanzo-pals (->> "resources/sanzo-colors.edn" 
                     slurp
                     read-string
                     :combos
                     (mapv :hsb)))
(declare grf)
(declare grf-fn)
(def RATIO (/ (+ 1 (Math/sqrt 5)) 2))
;(def RATIO Math/PI) 
;(def RATIO (Math/sqrt 2)) 
;(def RATIO Math/E) 

(def palette
  (cycle [[249 187  78]
          [ 70 162 141]
          [220 112 100]]))

(defn setup []
  (q/no-loop)
  ;(q/set-state! :palette (cycle (rand-nth sanzo-pals)))
  (let [pal (shuffle (rand-nth sanzo-pals))
        bg (peek pal) fills (pop pal)]
    (q/set-state! :palette (rand-nth sanzo-pals)
                  :bg bg :fills (cycle fills))))
    

(defn setup-fn-mode []
  (q/background 255 255 236)
  (q/frame-rate 5)
  ;(q/set-state! :palette (cycle (rand-nth sanzo-pals)))
  {:i 0 :palette (cycle (rand-nth sanzo-pals))})

(defn update-sketch [state]
  ; increase radius of the circle by 1 on each frame
  (update-in state [:i] inc))
(def letters (mapv str "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ"))
(defn draw []
  (q/ellipse-mode :radius)
  (q/rect-mode :center)
  (q/no-stroke)
  ;(q/background 255 255 236)
  (apply q/background (q/state :bg))
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (doseq [i (range 1000)]
      (let [v i;(+ (mod (q/frame-count) 3) i)
            ang (* v RATIO q/TWO-PI) ;angle increases in steps of phi
            r   (* 1.05 (Math/sqrt v) (q/width) (/ 70))
            x   (* (q/cos ang) r)
            y   (* (q/sin ang) r)
            sz  (+ 3 (* i 0.005))
            corner-x (case (mod i 4) 0 (- (/ (q/width) 2)) 1 (/ (q/width) 2) 2 (/ (q/width) 2) 3 (- (/ (q/width) 2)))
            corner-y (case (mod i 4) 0 (- (/ (q/height) 2)) 1 (- (/ (q/height) 2)) 2 (/ (q/height) 2) 3 (/ (q/height) 2))]
        ;(q/stroke-weight 3)
        (apply q/stroke (conj (nth (q/state :fills) i)))
        ;; fill shape or not
        (if (< (rand) 0.2) (q/no-fill) (apply q/fill (nth (q/state :fills) i)))
        ;(q/text-size 20)
        ;(q/text (rand-nth letters) x y)
        (q/push-matrix)
        ;;ratet how?
        (q/rotate ang) 
        ;(q/rotate (* (rand) q/QUARTER-PI 0.0))
        ;; circle or square
        (if (< (rand) 0.5) (q/rect x y (* 2 sz) (* 2 sz)) (q/ellipse x y sz sz))
        (q/pop-matrix)))))
        ;(when (> (rand) 0.2) (q/ellipse x y sz sz))))))
        ;(q/line corner-x corner-y  x y)))))

(q/defsketch grf
  ;:host "host"
  :size [800 800]
  :setup setup
  :draw draw)

(defn draw-fn [state]
  (q/ellipse-mode :radius)
  (q/no-stroke)
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (let [i (:i state)
          v (+ (mod (q/frame-count) 3) i)
          ang (* v RATIO q/TWO-PI) ;angle increases in steps of phi
          r   (* (Math/sqrt v) (q/width) (/ 70))
          x   (* (q/cos ang) r)
          y   (* (q/sin ang) r)
          sz  (+ 3 (* i 0.005))]
      (apply q/fill (nth palette i))
      (q/ellipse x y sz sz))))

#_(q/defsketch grf-fn
    ;:host "host"
    :size [800 800]
    :setup setup-fn-mode
    :draw draw-fn
    :update update-sketch
    :middleware [m/fun-mode])
