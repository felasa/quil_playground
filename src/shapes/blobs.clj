(ns shapes.blobs
  (:require [shapes.polygons :refer [close-path n-gon]]
            [util.transform :refer [mutate-path]]))

(defn blob 
  [position scale stds annealing mutations]
  (let [ini-shape (close-path (n-gon scale 8))]
    (map #(map + % position) (mutate-path mutations annealing stds ini-shape))))

