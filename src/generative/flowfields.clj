(ns generative.flowfields
  (:require [quil.core :as q]
            [clojure.data.json :as json]
            [util.fields :as fields])) 
            ; [perlin-noise perlin-field curl-field grid-field]]))

;; TODO: 
;; - CLEANUP it's a mess rn
;; - Really need to cleanup
;; - fn to handle next curve position (avoid collisions/spacing)
;; - draw circle instead of line segment [done]
;; - color handling
;; - funs that return data for drawing. 
;;     updte maybe make draw functions genrate data first))

(def pallete [[0xcc 0x12 0x36] [0xfd 0xbe 0x68] [0x00 0x97 0x8c]])

(defn rand-nextxy
  []
  [(rand-int (q/width)) (rand-int (q/height))])

(defn grid-nextxy
  [step iter]
  (let [x (mod (* step iter) (q/width))
        y (* step (quot (* step iter) (q/height)))]
    [x y]))

(defn scale-coord
  [max-value resolution value]
  (* (int (/ value max-value resolution)) max-value resolution))

(defn scale-margin 
  "not implemented. wnat to scale coordinates but add a margin outisde of the viewport"
  ([factor p] (->> p (map #(* factor %)) (map + [(/ (q/width) 4) (/ (q/height) 4)]))))

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
           [x' y'] (mapv + [x y] (map #(* step-length %) [dx dy]))
           d (q/dist x y x' y')]
        ;(q/ellipse x y (/ d 2) (/ d 2))
        ;(q/ellipse x y 2 2)
        ;(q/line x y x' y')
       (segment-fn x y x' y')
       (recur (dec n)
              x' y'
              (conj path [x' y'])))
     path)))

(defn distanced? [radius path x y]
  (not-any? #(> radius (q/dist x y (get % 0) (get % 1)))
            path))

(defn drw
  "draws n-curves curves using field-fn"
  [field-fn segment-fn n-segments segment-length nextxy-fn n-curves]
  (q/no-loop)
  ;(q/stroke 2)
  ;(q/no-stroke)
  (q/no-fill)
  ;(q/stroke 200)
  (q/stroke-weight 1)
  ;(q/point col row)
  (loop [n 0
         drawn []
         tries 500]
    (if (and (< n n-curves) (> tries 0))
      (let [[x y] (nextxy-fn n)
            color (rand-nth pallete)]
        (if (distanced? 0 drawn x y)
          (do
            ;(q/stroke (color 0) (color 1) (color 2) 255)
            ;(q/fill (color 0) (color 1) (color 2) 255)
            (let [pts (draw-curve-with-field field-fn segment-fn
                                             n-segments segment-length x y)]
              (recur (inc n) (into drawn pts) 500)))
          (recur n drawn (dec tries))))
      nil)))

(defn setup []
  (q/background 50)
  (q/stroke 0)
  (q/stroke-join :round))

(defn repel-potential [cx cy R]
  (fn [x y]
    (let [d (q/dist x y cx cy)
          dn (/ d (* 2 R))]
      (cond (<= d R) 0
            :else (* dn dn)))))
(comment
  (declare example flowviz)
  (let [params
        {:Width 900
         :Height 900
         :grid-fraction 2
         :perlin-scale 0.005
         :n-segments 800
         :segment-length 7
         :n-curves 1831
         :g-nextxy-step 17}
        {:keys [Width Height grid-fraction perlin-scale n-segments
                segment-length n-curves g-nextxy-step]} params
        #_(field-fn (grid-field (partial scale-coord Width (/ grid-fraction))
                                (curl-field perlin-scale)))
                               ;(perlin-field perlin-scale)

        #_(field-fn (fields/grid-field
                      (partial scale-coord Width (/ grid-fraction))
                      (fields/perlin-field perlin-scale)))
        #_(field-fn (curl-field (fn [x y] (* (q/noise x y) (/ (inc x)) (/ (inc y))))
                                perlin-scale))
        #_(field-fn (curl-field (fn [x y] (* (q/noise x y) ((repel-potential 450 450 200) x y)))
                                perlin-scale))
        field-fn (fields/curl-field 0.005)
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
      :settings #(q/smooth 8)
      :setup setup
      ;:draw (fn [] (q/line 0 0 800 800))
      :draw #(drw field-fn q/line 500 5 rand-nextxy 100)
      :draw-not (fn [] (do
                        (q/no-loop)
                        (q/color-mode :hsb 359 1 1 1)
                        (doseq [x (range 0 900)
                                y (range 0 900)]
                          (q/set-pixel x y
                            (q/color 180 0.5 ((fields/modulate
                                               (fields/perlin-noise perlin-scale)
                                               (fn [x y] (let [d (/ (q/dist x y 450 450) 637)] (* d d)))) 
                                              x y))))
                        (q/color-mode :rgb 255 255 255 1)
                        (apply q/stroke (get pallete 0)) 
                        (drw (comp 
                               (fields/gen-curl
                                 (fields/modulate (fields/perlin-noise perlin-scale)
                                                  ;(comp (fn [z] (q/sqrt z)))
                                                  ;  (fn name [x y] (abs (/ (- y 450.0) 450)))))))
                                                  (fn [x y] (let [d (/ (q/dist x y 450 450) 637)] (* d d))))))
                             segment-fn
                             n-segments segment-length
                             nextxy-fn
                             n-curves)
                        (apply q/stroke (get pallete 1)) 
                        (drw (comp 
                               (fields/gen-curl
                                 (fields/modulate (fields/perlin-noise perlin-scale)
                                                  (fn [x y] (let [d (/ (q/dist x y 450 450) 637)] (* d d))))))
                                                  ;(fn name [x y] (abs (/ (- y 450.0) 450))))))
                             segment-fn
                             n-segments segment-length
                             (fn [iter] 
                               (let [[x y] ((partial grid-nextxy g-nextxy-step) iter)]
                                 [(- 900 x) (- 900 y)]))
                             n-curves)))

      :size [Width Height]
      :features [:resizable]
      :renderer :java2d))
  (q/defsketch flowviz
    :title "bin"
    :setup setup
    :settings #(q/smooth 16)
    :size [900 900]
    :draw (fn []
            (q/no-loop)
            (q/color-mode :hsb 359 1 1 1)
            (doseq [x (range 0 900)
                    y (range 0 900)]
              (q/set-pixel x y (q/color 53 ((fields/perlin-noise 0.01) x y) 1)))
            (doseq [x (range 0 (q/width) 90)
                    y (range 0 (q/height) 90)]
              (draw-curve-with-field (fields/curl-field 0.005) q/line
                                     500 5
                                     x y)))
    :renderer :p2d))

(defn sink 
  "tries to get a field to converge to the center. ideally when further there's more
   variation but near the center it draws closer"
  []
  (q/no-loop)
  (q/background 50)
  (q/noise-seed (System/nanoTime))
  (q/stroke 250)
  (dotimes [i 500]
    ;(apply q/stroke (rand-nth pallete))
    (draw-curve-with-field 
      ;(fields/perlin-field 0.005)
      #_(fn [x y] 
          (let [hx (/ (q/width) 2) hy (/ (q/height) 2)
                ;d  (/ (q/dist x y hx hy) 637.0)
                [nx ny] (mapv #(/ % 900.0) (mapv - [x y] [hx hy]))]
            [ny (- nx)]))
      #_(fields/avg-fields
          (fn [x y] 
            (let [hx (/ (q/width) 2) hy (/ (q/height) 2)
                  ;d  (/ (q/dist x y hx hy) 637.0)
                  [nx ny] (mapv #(/ % 900.0) (mapv - [x y] [hx hy]))]
              [ny (- nx)]))
          ;(fields/perlin-field 0.005)
          (fn [x y] 
            (let [hx (/ (q/width) 2) hy (/ (q/height) 2)]
                  ;d (/ (q/dist x y hx hy) 367)]
               (map - [x y] [hx hy]))))
      (fields/add-fields 
        (comp (fn [V] (mapv #(* % 10) V)) (fields/curl-field 0.009))
        (fn [x y] (let [d (q/dist x y 450 450)] 
                    (mapv #(/ % (q/sqrt  d)) (map - [450 450] [x y])))))
      q/line
      100
      5
      (rand-int (q/width))
      (rand-int (q/height)))))

(comment 
  (declare sktch bg)
  (q/defsketch sktch
    :size [900 900]
    :title "Title"
    :settings #(q/smooth)
    :setup setup
    :draw sink)
  (q/defsketch bg
    :size [1600 900]
    :title "Title"
    :settings #(q/smooth)
    :setup (fn[] (q/no-loop) (q/background 50))
    :draw (fn []
            (q/stroke 250)
            (q/stroke-weight 2)
            (dotimes [i 200] 
              (let [x (rand-int (q/width)) y (rand-int (q/height))]
                (draw-curve-with-field (fields/perlin-field 0.006)
                                       q/line 100 5 x y))))))

;(use :reload 'generative.flowfields)
;(.redraw sktch)

;Example drawing composing field with grid 
;(drw (grid-field (partial perlin-field 0.005 
;                 (partial scale-coord 800 (/ 20))
;     900))
(comment
  (quil.applet/with-applet generative.flowfields/example
     (let [hash (subs (str (random-uuid)) 0 5)]
       (spit (str "out/flowfields/flowfield_" hash ".json") (json/write-str params))
       (q/save (str "out/flowfields/flowfield_" hash ".png")))) ;
  (quil.applet/with-applet generative.flowfields/flowviz
    (let [hash (subs (str (random-uuid)) 0 5)]
      (q/save (str "out/flowfields/flowviz_" hash ".png")))) ;
  (quil.applet/with-applet generative.flowfields/sktch
    (let [hash (subs (str (random-uuid)) 0 5)]
      (q/save (str "out/flowfields/flowviz_sink_" hash ".png")))) ;
  (quil.applet/with-applet generative.flowfields/bg
    (let [hash (subs (str (random-uuid)) 0 5)]
      (q/save (str "out/flowfields/bg.png"))))) ;
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
