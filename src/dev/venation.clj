(ns dev.venation
  (:require [quil.core :as q]
            [clojure.math :as math]))

(defn sq-dist
  "Square distance"
  [p1 p2]
  (->> (map - p1 p2) 
       (map #(* % %))
       (reduce +)))

(defn v-dif 
  "Vector difference"
  [p1 p2]
  (mapv - p1 p2))

(defn v-sum 
  "Vector sum"
  [p1 p2]
  (mapv + p1 p2))

(defn normalize 
  "Make univ vector"
  [p]
  (let [norm (math/sqrt (sq-dist p [0 0]))]
    (mapv / p (repeat 2 norm))))
  
(sq-dist (normalize [1 1]) [0 0])
(math/sqrt (sq-dist [(/ 2) (/ 2)] [0 0]))
(def ex-veins {[400 400] nil
               [350 400] [400 400]
               [400 450] [400 400]
               [350 450] [350 400]
               [450 450] [400 450]})

(def ex-auxins
  (->> (fn [] (vector (rand-int 800) (rand-int 800)))
       (repeatedly) 
       (take 5)))

(def ex-auxin (first ex-auxins)) 

(reduce 
  (fn [L r] ) {}
  (apply min-key val 
    (reduce (fn [L v] (assoc L v (sq-dist ex-auxin v))) {}
        (keys ex-veins))))

(defn av-dists [veins a]
  (reduce (fn [L v] (assoc L v (sq-dist a v))) {}
          (keys veins)))

(comment (av-dists ex-veins (first ex-auxins)))

(defn auxin-distances 
  [auxins veins]
  (->> (map (fn [a] {a (av-dists veins a)}) auxins)
       (reduce into {})))
  
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
                          (take spawn-rate)))
        distances 
        (->> (auxin-distances auxins veins)
             (filter (fn [kv] (every? #(> % R-exclusion) (vals (val kv)))))
             (reduce into)
             (apply hash-map))
        filtered (keys distances)
        assigned (update-vals distances  min-by-val)
        directions (update-vals (->> (group-by #(assigned %) (keys assigned))
                                     (map (fn [kv] (hash-map (key kv) (mapv (fn [p] (v-dif (key kv) p)) (val kv))))) 
                                     (reduce conj))
                                (fn [coll] (mapv #(* segment-len %) (normalize (reduce v-sum coll)))))
        nv (->> directions 
                (map (fn [kv] {(mapv + (key kv) (val kv)) (key kv)}))
                (into veins))
        return {:auxins filtered :veins nv}]
    ;filtered))
    ;distances))
    ;assigned
    ;directions))
    ;nv
    return))

(comment  (step-grow 15000 50 20 {:auxins ex-auxins :veins ex-veins}))

(def parametrized (partial step-grow 150000 100 25))
(parametrized (parametrized {:auxins [] :veins ex-veins}))

(defn gen-veins [iters]
  (loop [iter 1
         state {:auxins [] :veins {[400 400] nil}}]
    (if (< iter iters)
      (recur (inc iter) (parametrized state))
      state)))


(def algo (gen-veins 100))
(first (:veins algo))
(declare skveins)
(q/defsketch skveins
  :setup (fn [] (q/no-loop))
  :size [800 800]
  :draw (fn [] 
          (let [veins (remove #(nil? (val %)) (:veins (gen-veins 100)))]
            (doseq [e veins]
              (q/ellipse ((key e) 0) ((key e) 1) 10 10))))) 

(doseq [v (:veins algo)]
  (println (v 0) (v 1)))
