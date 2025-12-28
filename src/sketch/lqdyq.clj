(ns sketch.lqdyq
  (:require [quil.core :as q]
            [shapes.blobs :as blobs]
            [generative.watercolor :as blb]
            [util.masking :as msk]))

(declare sketch)

(defn setup []
  (q/no-loop))

(def ex-conds 
  {:direction (rand 6.24);q/HALF-PI
   :position [200 50] :lenght 180 :end? false 
   :width 10 :generation 1})

(defn branch [conditions]
  (loop [state-q (list conditions)
         ret [conditions]]
    (if (empty? state-q) ret
      (let [{:keys [direction position lenght end? width generation]}
            (first state-q)
            next-pos (mapv + position [(* lenght (q/cos direction))
                                       (* lenght (q/sin direction))])
            next-direction-l (+ direction (* 0.5 (q/random-gaussian)))
            next-direction-r (+ direction (* 0.5 (q/random-gaussian)))
            next-lenght-l (+ (* 2 (q/random-gaussian)) (/ lenght 1.618))
            next-lenght-r (+ (* 2 (q/random-gaussian)) (/ lenght 1.618))
            next-generation (inc generation)
            next-width (* width 0.8)
            next-end? (or (< (rand (/ generation)) 0.04)
                          (< next-width 2))]
        (if end? (recur (rest state-q) ret)
           (let [childs (list {:direction next-direction-l
                               :position next-pos :lenght next-lenght-l
                               :end? next-end? :width next-width 
                               :generation next-generation}
                              {:direction next-direction-r
                               :position next-pos :lenght next-lenght-r
                               :end? next-end? :width next-width 
                               :generation next-generation})]
             ;(println next-generation)
             (recur (into (rest state-q) childs)
                    (into ret childs))))))))

(defn branch2pts [branch]
  (loop [rem branch
         pts []]
    (if (empty? rem) pts
      (let [current (first branch)
            {:keys [position end?]} current]
        (if end? 
          (recur (list) (conj pts position))
          (recur (rest branch) (conj pts position)))))))
        
(defn draw-branch [branches]
  ;(q/color-mode :hsb 359 100 100 1.0) 
  ;(q/stroke 20 70 49) 
  ;(q/stroke 40 31 92) 
  ;(q/stroke 16 50 34) 
  ;(q/fill 20 70 49) 
  (loop [rem branches]
    (if (empty? rem) nil
      (let [branch (first rem)
            {:keys [direction position lenght end? width generation]} branch]
        (q/stroke-weight width)
        (if end? 
          (do 
            (q/ellipse (position 0) (position 1)
                       (* 2 width)
                       (* 2 width))
            (recur (rest rem)))
          (do 
            (q/line position
                    (mapv + position
                          [(* lenght (q/cos direction))
                           (* lenght (q/sin direction))]))
            (recur (rest rem))))))))
;  (q/stroke 20 60 79 0.5) 
;  (loop [rem
;         (map (fn [m] (update m :width (fn [w] (max (- w 3) 1)))) branches)]
;    (if (empty? rem) nil
;      (let [branch (first rem)
;            {:keys [direction position lenght end? width generation]} branch]
;        (q/stroke-weight width)
;        (if end? 
;          (do 
;            #_(q/ellipse (position 0) (position 1) 5 5)
;            (recur (rest rem)))
;          (do 
;            (q/line position
;                    (mapv + position
;                          [(* lenght (q/cos direction))
;                           (* lenght (q/sin direction))]))
;           (recur (rest rem))))))))
        
(defn draw-fn []
  (let [branch-layer (q/create-graphics 800 800)
        texture-layer (q/create-graphics 800 800)
        shpl (q/create-graphics 800 800) mskl (q/create-graphics 800 800)]
    (q/with-graphics branch-layer 
      (q/background 255 0)
      (q/color-mode :hsb 359 100 100 1.0)
      (q/stroke 0 0 0)
      (q/fill 0 0 0)
      ;(q/stroke 40 31 92) 
      ;(q/fill 40 31 92) 
      (dotimes [theta 10] 
        (let [R 90 angle (+ theta  (/ q/TWO-PI 10))
              dx (* R (q/cos angle)) 
              dy (* R (q/sin angle))
              position (mapv + [dx dy] [400 400])
              branches (branch 
                        {:direction (+ angle (* 0 (q/random-gaussian)))
                         :position (mapv + position [(* 5 (q/random-gaussian)) (* 5 (q/random-gaussian))])
                         :lenght 100 :end? false 
                         :width 10 :generation 1})]
          (draw-branch branches))))
    (q/with-graphics texture-layer 
      (q/background 255 0)
      (blb/draw-blob 600 4 30 [255 1] [400 400]))
    (q/with-graphics shpl
      (q/background 255 0)
      (q/fill 46 27 40)
      (q/rect 100 100 300 300))
    (q/with-graphics mskl 
      (q/background 255 0)
      (q/fill 255)
      (q/rect 150 150 200 200))
    ;(q/image mskl 0 0)))
    ;(q/image texture-layer 0 0)))
    (msk/mask-w-alpha shpl mskl)))
  
(q/defsketch sketch
  :size [800 800]
  :settings #(q/smooth)
  :setup (fn [] (q/no-loop) 
           (q/color-mode :hsb 359 100 100 1.0)
           (q/background 250))
  :renderer :java2d        
  :draw draw-fn
  :drawa #(do 
            (q/color-mode :hsb 359 100 100 1.0)
            ;(blb/draw-blob 600 5 30 [305 55 39 0.1] [400 400])
            (dotimes [theta 10] 
              (let [R 90 angle (+ theta  (/ q/TWO-PI 10))
                    dx (* R (q/cos angle)) 
                    dy (* R (q/sin angle))
                    position (mapv + [dx dy] [400 400])
                    branches (branch 
                              {:direction (+ angle (* 0 (q/random-gaussian)))
                               :position (mapv + position [(* 5 (q/random-gaussian)) (* 5 (q/random-gaussian))])
                               :lenght 100 :end? false 
                               :width 10 :generation 1})]
               (draw-branch branches)))
            (doseq [x (range 800) y (range 800)]
              (when (< (rand) 0.2) (q/set-pixel x y (q/color 0 0 0))))))
           ;(blb/draw-blob 100 4 40 [0 0 100 0.05] [400 400])))
  
           
  ;:dra #(test-field 0.003)
  ;:dra #(check-field 0.005 10))
(quil.applet/with-applet sketch.lqdyq/sketch 
  (branch ex-conds))
