(ns sketches.venas
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [util.core :refer [dist vector-add normalize hex-to-rgb]]))

(defn v-dif 
  "Vector difference"
  [p1 p2]
  (mapv - p1 p2))

(defn av-dists [veins a]
  (reduce (fn [L v] (assoc L v (dist a v))) {}
          (keys veins)))


(defn auxin-distances 
  [auxins veins]
  (->> (map (fn [a] {a (av-dists veins a)}) auxins)
       (reduce into {})))
  
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
             ;#((update-vals % (fn [m] (remove (fn [e] (zero? (val e))) m))))
             (filter (fn [kv] (every? #(> % R-exclusion) (vals (val kv)))))
             (reduce into)
             (apply hash-map))
        filtered (keys distances)
        assigned (update-vals distances min-by-val)
        directions (update-vals
                     (->> (group-by #(assigned %) (keys assigned))
                          (map (fn [kv] (hash-map (key kv) (mapv (fn [p] (v-dif p (key kv))) (val kv))))) 
                          (reduce conj))
                     (fn [coll] (mapv #(* segment-len %) (normalize (reduce vector-add coll)))))
        nv (->> directions 
                (map (fn [kv] {(mapv long (mapv + (key kv) (val kv))) (key kv)}))
                (into veins))
        return {:auxins filtered :veins nv}]
    return))

(def sanzo (read-string (slurp "resources/sanzo-colors.edn")))
(defn palette-fn []
  (->> sanzo :combos 
      (map :hex)
      (filter #(= (count %) 3))
      (rand-nth)
      (mapv hex-to-rgb)
      (shuffle))) 

(defn setup-loop []
  (q/frame-rate 12)
  ;(q/background 214,180,62)
  (let [pal (palette-fn)] 
    (apply q/background (pal 0))
    {:auxins [] :veins {[400 400] nil}
     :iter 1
     :palette pal}))

;Meant to visualize animated step by step. todo: check if still working
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


;; 4 main variations;
;; - only single circles
;; - with random concentrics
;; - with random fill
;; - with random segments
(defn draw-param
  [R-exclusion step-length palette stem-rate alt-color-rate base-size fill-rate b0-rate b1-rate b2-rate b3-rate] 
  (q/no-fill)  
  (q/ellipse-mode :radius)
  (q/color-mode :rgb 255 255 255 1)
  (let [par-fn (partial step-grow R-exclusion step-length 5)
        gen-veins (fn [iters state]
                    (loop [iter 1 state state]
                      (if (< iter iters) (recur (inc iter) (par-fn state)) state)))
        veins
        (:veins
          (gen-veins 100 
                     {:auxins {} 
                      :veins {[(+ 200 (rand-int 400)) (+ 200 (rand-int 400))] nil}})) 
        pal (shuffle palette)
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
        (when (and (< (rand) stem-rate) dest) (q/line orig dest)) 
        (if (< (rand) alt-color-rate) (apply q/stroke fg2) (apply q/stroke fg1))
        (when (< (rand) b0-rate) 
          (when (< (rand) fill-rate) (apply q/fill fg2))
          (q/ellipse (orig 0) (orig 1) base-size base-size) 
          (q/no-fill))
        (if (< (rand) alt-color-rate) (apply q/stroke fg2) (apply q/stroke fg1))
        (when (< (rand) b1-rate) (q/ellipse (orig 0) (orig 1) (* 3 (/ base-size 4)) (* 3 (/ base-size 4)))) 
        (if (< (rand) alt-color-rate) (apply q/stroke fg2) (apply q/stroke fg1))
        (when (< (rand) b2-rate) (q/ellipse (orig 0) (orig 1) (* 2 (/ base-size 4)) (* 2 (/ base-size 4)))) 
        (if (< (rand) alt-color-rate) (apply q/stroke fg2) (apply q/stroke fg1))
        (when (< (rand) b3-rate) (q/ellipse (orig 0) (orig 1) (* 1 (/ base-size 4)) (* 1 (/ base-size 4)))))))) 

(defn venas [r-exclusion step-length pal stem-rate 
             alt-color-rate base-size fill-rate
             b0-rate b1-rate b2-rate b3-rate]
  (draw-param r-exclusion step-length pal stem-rate alt-color-rate
                  base-size fill-rate b0-rate b1-rate b2-rate b3-rate))

(defn canon-sketch [] 
  (venas (+ 10 (rand-int 50))
         (+ (/ (+ 10 (rand-int 50)) 2) (- (rand-int 40) 20))
         (palette-fn)
         (rand) 
         (rand)
         (- (/ (+ (/ (+ 10 (rand-int 50)) 2) (- (rand-int 40) 20)) 2) 1)
         (rand)
         (rand)
         (rand)
         (rand) 
         (rand)))
 
(defn multi-draw [times save?]
  (dotimes [i times]
    (let [r-exclusion (+ 10 (rand-int 50))
          step-length (+ (/ r-exclusion 2) (- (rand-int 40) 20))
          pal (palette-fn) stem-rate (rand) 
          alt-color-rate (rand) base-size (- (/ step-length 2) 1)
          fill-rate (rand) b0-rate (rand) b1-rate (rand) b2-rate (rand) 
          b3-rate (rand)]
      (try 
        (draw-param r-exclusion step-length pal stem-rate alt-color-rate
                    base-size fill-rate b0-rate b1-rate b2-rate b3-rate)
        (when save? (let [fname (str "out/veins/2/veins_" (format "%05d" i) ".png")]
                      (q/save fname)))
        (catch Throwable t 
          (println "Error: " t))))))
 
(declare gen-variations)
(comment (q/defsketch gen-variations 
           :settings #(q/smooth 16)
           :setup (fn [] (q/no-loop) (q/hint :disable-async-saveframe))
           :size [800 800]
           :draw #(multi-draw 1 false))) 

