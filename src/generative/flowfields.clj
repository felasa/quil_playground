(ns generative.flowfields
  (:require [quil.core :as q]
            [clojure.math :as math]
            [util.core :refer [local-context]]
            [clojure.data.json :as json]
            [util.fields :as fields :refer [perlin-field curl-field grid-field]]))

;; TODO: 
;; - pass curve-fn to draw
;; - fn to handle next curve position (avoid collisions/spacing)
;; - draw circle instead of line segment [done]
;; - color handling
;; - funs that return data for drawing

(def pallete [[0xcc 0x12 0x36] [0xfd 0xbe 0x68] [0x00 0x97 0x8c]])

(defn rand-nextxy
  [iter]
  [(rand-int (q/width)) (rand-int (q/height))])

(defn grid-nextxy
  [step iter]
  (let [x (mod (* step iter) (q/width))
        y (* step (quot (* step iter) (q/height)))]
    [x y]))

(defn draw-curve
  [x y x' y']
  (q/line x y x' y'))

(defn draw-circles
  [scale x y x' y']
  (let [d (* scale (q/dist x y x' y'))]
    (q/ellipse x y d d)))

(defn draw-curve-with-field
  "Draws a curve given start coordinates, number of segments, segment lenght initial stroke
   Takes a function representing a flow field at a downsampled grid. column, row coordinates"
  [field-fn segment-fn n-segments step-length start-x start-y]
  (loop [n n-segments
         x start-x y start-y
         path [[x y]]]
    (if (> n 0)
      (let [;angle (field-fn x y)
            [dx dy] (field-fn x y)
            [x' y'] (mapv + [x y] (map #(* step-length %) [dx dy]))]
            ;d (q/dist x y next-x next-y)]
         ;(q/ellipse x y (/ d 2) (/ d 2))
         ;(q/ellipse x y 2 2)
         ;(q/line x y next-x next-y)
         (segment-fn x y x' y')
         (recur (dec n) 
                x' y'
                (conj path [x' y'])))
      path)))

(defn distanced? [radius path x y]
  (not-any? #(> radius (q/dist x y (get % 0) (get % 1))) path))

(defn drw
  "draws n-curves curves using field-fn"
  [field-fn segment-fn n-segments segment-length nextxy-fn n-curves]
  (q/no-loop)
  ;(q/stroke 2)
  ;(q/no-stroke)
  ;(q/no-fill)
  (q/stroke 200)
  (q/stroke-weight 1)
  ;(q/point col row)
  (loop [n 0 
         drawn []
         tries 500]
    (if (and (< n n-curves) (> tries 0))
      (let [;x (rand-int (q/width))
            ;y (rand-int (q/height))
            [x y] (nextxy-fn n)
            color (rand-nth pallete)]
        (when true;(distanced? 0 drawn x y)
          ;(q/stroke (color 0) (color 1) (color 2) 255)
          ;(q/fill (color 0) (color 1) (color 2) 255)
          (let [pts (draw-curve-with-field field-fn segment-fn 
                                           n-segments segment-length x y)]
            (recur (inc n) (into drawn pts) 500)))
        (recur n drawn (dec tries)))
      nil)))

(defn setup []
  (q/smooth)
  (q/background 50)
  (q/stroke 0)
  (q/stroke-join :round))

(comment
  (let [params 
        {:Width 900
         :Height 900
         :grid-fraction 90
         :perlin-scale 0.005
         :n-segments 400
         :segment-length 1
         :n-curves 1600
         :g-nextxy-step 23}
        {:keys [Width Height grid-fraction perlin-scale n-segments
                segment-length n-curves g-nextxy-step ]} params
        #_(field-fn (grid-field (partial scale-coord Width (/ grid-fraction))
                                (curl-field perlin-scale)
                               ;(perlin-field perlin-scale)

        ;field-fn (grid-field (partial scale-coord Width (/ grid-fraction))
                             (curl-field perlin-scale)))
        field-fn (curl-field perlin-scale)
        ;field-fn (perlin-field perlin-scale)
        ;field-fn (fields/circle-field 40 450 450)
        #_(field-fn (fields/sum-fields 
                      (partial perlin-field perlin-scale)
                      ;(partial curl-field perlin-scale))
                      (fields/circle-field 80 450 450)))
        ;nextxy-fn rand-nextxy
        nextxy-fn (partial grid-nextxy g-nextxy-step)
        ;n-curves (* (/ Width g-nextxy-step) (/ Height g-nextxy-step))
        ;segment-fn (partial draw-circles 0.5)
        segment-fn draw-curve]
    (def params params)
    (q/defsketch example
      :title "Title"
      ;:display 1
      :settings #(q/smooth)
      :setup setup
      ;:draw (fn [] (q/line 0 0 800 800))
      :draw #(drw field-fn
                  segment-fn
                  n-segments segment-length
                  nextxy-fn
                  n-curves)
      :size [Width Height]
      :features [:resizable]
      :renderer :java2d)))

;Example drawing composing field with grid 
;(drw (grid-field (partial perlin-field 0.005 
;                 (partial scale-coord 800 (/ 20))
;     900))
(comment
  (quil.applet/with-applet generative.flowfields/example
    (let [hash (subs (str (random-uuid)) 0 5)]
      (spit (str "out/flowfields/flowfield_" hash ".png") (json/write-str params))
      (q/save (str "out/flowfields/flowfield_" hash ".png"))))) ;

; to see it happening live
#_((defn draw-animated 
    [field-fn step-size n-segments]
    (let [x (atom 400) 
          y (atom 400)
          segment-count (atom 0)]
      (q/sketch
        :draw 
        (fn []
          (if (<= @segment-count n-segments)
            (let [theta (field-fn @x @y)
                  x' (+ @x (* step-size (math/cos theta)))
                  y' (+ @y (* step-size (math/sin theta)))]
              (q/line @x @y x' y')
              (reset! x x')
              (reset! y y')
              (swap! segment-count inc)) 
            (do (reset! x (rand-int (q/height)))
                (reset! y (rand-int (q/width)))
                (reset! segment-count 0)))) 
        :settings #(q/smooth 8)
        :setup (fn [] 
                (q/frame-rate 500)
                (q/background 255)
                (q/stroke 0)
                (q/stroke-join :round))
        :size [800 800])))) 
