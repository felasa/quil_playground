(ns sketch.veins
  (:require [quil.core :as q]))


(defn algo [p-ini vein-radius intrusion-radius r3 r4]
  (let [auxins []
        stem [p-ini]]
    (loop [aux 
           (->> (repeatedly (fn [ ] (vector (rand-int 800) (rand-int 800))))
                (take 10))
           veins stem
           iter 1]
      (when (<= iter 100)
      ;;find veins closest to auxins
        (map (fn [a] (map (fn [v] (distance a v)  ) veins))
             aux)))))
      ;;compute vector and average
      ;;normalize
      ;;add vein in that direction
      ;;remove auxins that intersect with intrusion-radius
      ;;add more random auxins
      ;;at some point draw


(apply min-key [0 1 2 3 4 5] [4 3 2 1])
([1 2 3] 1)

