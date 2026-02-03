(ns dev.venation
  (:require [quil.core :as q]
            [clojure.math :as math]
            [quil.middleware :as m]
            [util.color :as color]))

(defn dist
  "Euclidian distance"
  [p1 p2]
  (->> (map - p1 p2) 
       (map #(* % %))
       (reduce +)
       (math/sqrt)))

(defn v-dif 
  "Vector difference"
  [p1 p2]
  (mapv - p1 p2))

(defn v-sum 
  "Vector sum"
  [p1 p2]
  (mapv + p1 p2))

(defn normalize 
  "Make unit vector"
  [p]
  (let [norm (dist p [0 0])]
    (mapv / p (repeat 2 norm))))
  
(def ex-veins {[400 700] nil
               [400 690] [400 700]
               [400 680] [400 690]})

(def ex-auxins
  (->> (fn [] (vector (rand-int 800) (rand-int 800)))
       (repeatedly) 
       (take 5)))

(defn av-dists [veins a]
  (reduce (fn [L v] (assoc L v (dist a v))) {}
          (keys veins)))

(comment (av-dists ex-veins (first ex-auxins)))

(defn v-dists [veins a]
  (reduce (fn [L v] (assoc L v (dist a v))) {}
          veins))

(defn auxin-distances 
  [auxins veins]
  (->> (map (fn [a] {a (av-dists veins a)}) auxins)
       (reduce into {})))
  
(comment (auxin-distances [[0 0]] {[1 2] nil [3 4] nil}))

(defn pair-distances
  [auxins veins]
  (for [auxin auxins vein veins]
    [auxin vein (dist auxin vein)]))

(v-dists [0 0] [[1 2] [3 4]])

(comment (->> (pair-distances [[0 0] [0 1]] [[0 1] [0 2]])
              (group-by first)))

(comment (auxin-distances ex-auxins ex-veins))
;; {[711 697]
;;  {[400 400] 184930,
;;   [350 400] 218530,
;;   [400 450] 157730,
;;   [350 450] 191330,
;;   [450 450] 129130},
;;  [117 161]
;;  {[400 400] 137210,
;;   [350 400] 111410,
;;   [400 450] 163610,
;;   [350 450] 137810,
;;   [450 450] 194410},
;;  [58 777]
;;  {[400 400] 259093,
;;   [350 400] 227393,
;;   [400 450] 223893,
;;   [350 450] 192193,
;;   [450 450] 260593},
;;  [128 31]
;;  {[400 400] 210145,
;;   [350 400] 185445,
;;   [400 450] 249545,
;;   [350 450] 224845,
;;   [450 450] 279245},
;;  [133 241]
;;  {[400 400] 96570,
;;   [350 400] 72370,
;;   [400 450] 114970,
;;   [350 450] 90770,
;;   [450 450] 144170}}
  

(defn min-by-val
  "Returns the key val which val is least"
  [m]
  (key (apply min-key val m)))

;Recursive Algo:
;Given parameters
;1) filter auxins closer than R
;2) Assign vein to remaining auxin
;3) Add new veins
;4) ???
;5) Profit


(defn step-grow 
  [R-exclusion segment-len spawn-rate state]
  (let [{:keys [auxins veins]} state
        auxins (into auxins 
                     (->> (fn [] (vector (rand-int 800) (rand-int 800)))
                          (repeatedly)
                          ;(remove (fn [[x y]] (or (< x 150) (> x 650) (< y 150) (> y 650))))
                          ;(remove #(or (> (sq-dist % [400 400]) 300)))
                                       ;(> (sq-dist % [0 400])   500)))
                          (take spawn-rate)))
        distances 
        (->> (auxin-distances auxins veins)
             (filter (fn [kv] (every? #(> % R-exclusion) (vals (val kv)))))
             (reduce into)
             (apply hash-map))
        filtered (keys distances)
        assigned (update-vals distances  min-by-val)
        directions (update-vals
                     (->> (group-by #(assigned %) (keys assigned))
                          (map (fn [kv] (hash-map (key kv) (mapv (fn [p] (v-dif p (key kv))) (val kv))))) 
                          (reduce conj))
                     (fn [coll] (mapv #(* segment-len %) (normalize (reduce v-sum coll)))))
        nv (->> directions 
                (map (fn [kv] {(mapv long (mapv + (key kv) (val kv))) (key kv)}))
                (into veins))
        return {:auxins filtered :veins nv}]
    ;filtered))
    ;distances))
    ;assigned
    ;directions))
    ;nv))
    return))

(defn v-step-grow 
  "Same as above but returns veins as a vector no a map"
  [R-exclusion segment-len spawn-rate state]
  (let [{:keys [auxins veins]} state
        auxins (into auxins 
                     (->> (fn [] (vector (rand-int 800) (rand-int 800)))
                          (repeatedly)
                          ;(remove #(or (> (sq-dist % [400 400]) 300)))
                                       ;(> (sq-dist % [0 400])   500)))
                          (take spawn-rate)))
        distances 
        (->> (auxin-distances auxins veins)
             (filter (fn [kv] (every? #(> % R-exclusion) (vals (val kv)))))
             (reduce into)
             (apply hash-map))
        filtered (keys distances)
        assigned (update-vals distances  min-by-val)
        directions (update-vals
                     (->> (group-by #(assigned %) (keys assigned))
                          (map (fn [kv] (hash-map (key kv) (mapv (fn [p] (v-dif p (key kv))) (val kv))))) 
                          (reduce conj))
                     (fn [coll] (mapv #(* segment-len %) (normalize (reduce v-sum coll)))))
        nv (->> directions 
                (map (fn [kv] {(mapv long (mapv + (key kv) (val kv))) (key kv)}))
                (into veins))
        return {:auxins filtered :veins nv}]
    ;filtered))
    ;distances))
    ;assigned
    ;directions))
    ;nv))
    return))

(comment  (step-grow 15 20 10 {:auxins ex-auxins :veins ex-veins}))
;; {:auxins
;;  ([411 444]
;;   [529 553]
;;   [400 517]
;;   [167 147]
;;   [384 429]
;;   [66 251]
;;   [215 148]
;;   [781 124]
;;   [774 654]
;;   [346 614]
;;   [269 279]
;;   [81 234]
;;   [77 15]
;;   [411 650]
;;   [616 727]),
;;  :veins
;;  {[400 700] nil,
;;   [400 690] [400 700],
;;   [400 680] [400 690],
;;   [396 660] [400 680],
;;   [419 702] [400 700]}}

(def parametrized (partial step-grow 45 25 5))
(parametrized (parametrized {:auxins [] :veins ex-veins}))

(defn gen-veins [iters state]
  (loop [iter 1
         state state]  
    (if (< iter iters)
      (recur (inc iter) (parametrized state))
      state)))

(parametrized {:auxins ex-auxins :veins ex-veins})

(def sanzo (read-string (slurp "resources/sanzo-colors.edn")))
(defn palette-fn []
  (->> sanzo :combos 
      (map :hex)
      (filter #(= (count %) 3))
      (rand-nth)
      (mapv color/hex-to-rgb)
      (shuffle))) 

(defn setup-loop []
  (q/frame-rate 12)
  ;(q/background 214,180,62)
  (let [pal (palette-fn)] 
    (apply q/background (pal 0))
    {:auxins [] :veins {[400 400] nil}
     :iter 1
     :palette pal}))

(defn looped-draw
  [state]
  ;(q/frame-rate 12)
  ;(q/background 214,180,62)
  (let [auxins (:auxins state)
        veins (:veins state)
        palette (:palette state)] 
    ;(apply q/background (palette 0)) 
    (q/no-stroke)
    ;(q/fill 255 0 0)
    (q/text-size 20)
    ;(q/text (str (q/frame-count)) 10 10)
    #_(doseq [p auxins]
       (q/ellipse (p 0) (p 1) 10 10))
    (doseq [e veins]
      (let [p1 (key e), p2 (val e)]
        (when (and p2 (> (- 15 (/ (:iter state) 20)) 0)) 
           (apply q/stroke (palette 1)) 
           ;(q/stroke 107,113,64)
           (q/stroke-weight (- 15 (/ (:iter state) 20))) 
           (q/line p1 p2)
           (when (> (- 11 (/ (:iter state) 20)) 0)
             (apply q/stroke (palette 2)) 
             (q/stroke-weight (max (- 11 (/ (:iter state) 20)) 1)) 
             (q/line p1 p2)))
           ;(q/stroke 0 0 0))))))
           ;(q/stroke-weight 1)
           ;(q/line p1 p2))))))
        (q/no-stroke)))))
        ;(q/ellipse (p1 0) (p1 1) 2 2)))))

(defn update-loop [R-exclusion segment-len spawn-rate]
  (fn [state]
    (let [ns ((partial step-grow R-exclusion segment-len spawn-rate) state)
          ni (inc (:iter state))]
      (assoc ns :iter ni :palette (:palette state)))))

(looped-draw {:auxins ex-auxins :veins ex-veins})
(def algo (gen-veins 100 {}))
(declare skveins)

(q/defsketch skveins
  :settings #(q/smooth 16)
  ;:middleware [m/fun-mode]
  ;:setup setup-loop
  ;:update (update-loop 6 5 1)
  ;:draw looped-draw
  ;; Incorporar parametros R exlusion (controla densidad), segment-len (distancia entre nodos), spawn (cuantas auxinas agregar)
  ;; ref: 5 25 5 funciona ok, 35 25 5 tbm, 45 25 5 idem
  ;; mayor densidad funciona con menor longitud? si no, no se percibe el patron de bifurcacion
  ;; El diametro del circulo menor al la longitud del segmento para evitar solapes
  ;; Otros parametros a aleatorizar:
  ;;   diametro de las elipses (mitades desde la mas grande)
  ;;   tasa de aparicion de cada diametro
  ;;   si se dibuja el relleno de cada circulo
  :setup (fn [] (q/no-loop))
  :size [800 800]
  :draw (fn [] 
          (q/no-fill)  
          (q/color-mode :rgb 255 255 255 1)
          (let [veins (:veins (gen-veins 100 {:auxins [] :veins {[(+ 300 (rand-int 200)) (+ 300 (rand-int 200))] nil}})) 
                pal (shuffle (palette-fn))
                bg (pal 0)
                fg1 (pal 1)
                fg2 (pal 2)]
            (apply q/background bg)
            ;(q/no-stroke)
            ;(apply q/fill bg)
            ;(q/rect 30 30 745 745)
            ;(apply q/stroke fg1)
            ;(doseq [x (range 800) y (range 800)] (when (< (rand) 0.005) (q/set-pixel x y (q/color 0 0 0))))
            (doseq [e veins]
              (let [[orig dest] e]       ;i (range (count veins))]
                ;(q/stroke-weight 2))))) 
                ;(q/line (key e) (val e))))))
                ;(q/line (key (nth veins i)) (val (nth veins i)))))))
                (when (and (< (rand) 0.2) dest) (q/line orig dest)) 
                (if (< (rand) 0.1) (apply q/stroke fg2) (apply q/stroke fg1))
                (when (< (rand) 0.6) 
                  (when (< (rand) 0.15) (apply q/fill fg2))
                  (q/ellipse (orig 0) (orig 1) 22 22) 
                  (q/no-fill))
                (if (< (rand) 0.1) (apply q/stroke fg2) (apply q/stroke fg1))
                (when (< (rand) 0.5) (q/ellipse (orig 0) (orig 1) 18 18)) 
                (if (< (rand) 0.1) (apply q/stroke fg2) (apply q/stroke fg1))
                (when (< (rand) 0.5) (q/ellipse (orig 0) (orig 1) 12 12)) 
                (if (< (rand) 0.1) (apply q/stroke fg2) (apply q/stroke fg1))
                (when (< (rand) 0.7) (q/ellipse (orig 0) (orig 1) 6 6))))))) 
  ;:middleware [m/fun-mode]
  ;:draw draw-changing
  ;:update update-widths
  ;:setup #(do (q/frame-rate 8) (identity ini-state)))

;; 4 main variations;
;; - only single circles
;; - with random concentrics
;; - with random fill
;; - with random segments

(def example-stuff (:veins (gen-veins 100 {:auxins {} :veins {[400 400] nil}})))

(defn iterate-width []
  {6 12 12 18 18 22 22 6})

(defn inc-frame [w]
  (if (= w 22) 12 22))

(def ini-state (update-vals example-stuff (fn [v] (rand-nth [22 12]))))
(defn update-widths [state] (update-vals state inc-frame))
(update-widths ini-state)
(defn draw-changing [state]
  (q/background 50)
  (q/no-fill)
  (q/stroke-weight 2)
  (doseq [node state]
    (let [[v w] node]
      (q/ellipse (v 0) (v 1) w w))))

(comment 
  (quil.applet/with-applet dev.venation/skveins
    (q/save "out/veins/fav_001.png")))
   

(defn draw [R-exclusion step-lenght palette stem-rate] 
    (q/no-fill)  
    (q/color-mode :rgb 255 255 255 1)
    (let [veins (:veins (gen-veins 100 {:auxins {} :veins {[400 400] nil}})) 
          pal (shuffle (palette-fn))
          bg (pal 0)
          fg1 (pal 1)
          fg2 (pal 2)]
      (apply q/background bg)
      (apply q/stroke fg1)
      (doseq [e veins]
        (let [[orig dest] e]       ;i (range (count veins))]
          ;(q/stroke-weight 2))))) 
          ;(q/line (key e) (val e))))))
          ;(q/line (key (nth veins i)) (val (nth veins i)))))))
          (when (and (< (rand) 0.1) dest) (q/line orig dest)) 
          (if (< (rand) stem-rate) (apply q/stroke fg2) (apply q/stroke fg1))
          (when (< (rand) 0.6) 
            (when (< (rand) 0.3) (apply q/fill fg2))
            (q/ellipse (orig 0) (orig 1) 22 22) 
            (q/no-fill))
          (if (< (rand) 0.1) (apply q/stroke fg2) (apply q/stroke fg1))
          (when (< (rand) 0.5) (q/ellipse (orig 0) (orig 1) 18 18)) 
          (if (< (rand) 0.1) (apply q/stroke fg2) (apply q/stroke fg1))
          (when (< (rand) 0.5) (q/ellipse (orig 0) (orig 1) 12 12)) 
          (if (< (rand) 0.1) (apply q/stroke fg2) (apply q/stroke fg1))
          (when (< (rand) 0.7) (q/ellipse (orig 0) (orig 1) 6 6)))))) 
 
  
 
 
(letfn [(foo [x] (inc x))]
  (foo 3))
