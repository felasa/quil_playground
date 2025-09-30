(ns dev.birbs
  (:require [quil.core :as q]
            [clojure.math :as math]))

(defn norm2 [p]
  (reduce + (map #(* % %) p)))

(defn dist-sq
  [p1 p2]
  (norm2 (map - p1 p2)))

(defn normalize
  [p]
  (case p
    [0 0] p
    (let [N (math/sqrt (norm2 p))]
      (map #(* % (/ N)) p))))

(defn vadd [p1 p2]
  (map + p1 p2))

(defn centroid
  ([] [0 0])
  ([ps]
   (let [N (count ps)]
     (->> ps (reduce #(map + %1 %2))
          (mapv #(/ % N))))))

(comment
  (centroid [[-1 0] [1 0] [0 -1] [0 1]]))

(def example-flock
  [{:id 1 :position [50 50] :velocity [0 0]}
   {:id 2 :position [70 70] :velocity [0 0]}
   {:id 3 :position [20 20] :velocity [0 0]}])

(defn update-velocity [flock id]
  (let [MAX-DISTANCE 1e3
        state (->> flock (filter #(= (:id %) id)) first)
        others (->> flock (remove #(= (:id %) id))
                    (map (fn [kv] {:dist-sq (dist-sq (:position state) (:position kv))
                                   :position (:position kv)
                                   :rvec (mapv - (:position state) (:position kv))})))
        centroid (->> others
                      (map :position)
                      (reduce (fn [[Lx Ly] [rx ry]] [(+ Lx rx) (+ Ly ry)]))
                      (mapv #(/ % (count others))))
        nearest (->> others
                     (remove (fn [kv] (> (:dist-sq kv) MAX-DISTANCE)))
                     (map (fn [kv] (mapv #(/ % (:dist-sq kv)) (:rvec kv))))
                     (reduce #(mapv + %1 %2) [0 0]))
        newv (mapv #(/ % 100) (mapv + (:velocity state) centroid nearest))]
    (assoc state :velocity newv
           :position (mapv + (:position state) newv))))

(defn uv [flock]
  (let [centroid (->> flock (map :position)
                      (reduce vadd [0 0])
                      (mapv #(/ % (count flock))))
        updated (map (fn [kv] (mapv - centroid (:position kv))) flock)]
    updated))

(comment
  (uv example-flock)
  (update-velocity
   [{:id 1 :position [50 50] :velocity [0 0]}
    {:id 2 :position [70 70] :velocity [0 0]}
    {:id 3 :position [20 20] :velocity [0 0]}]
   3))

(defn update-flock
  [flock]
  (map #(update-velocity flock %) (range 1 (inc (count flock))))
  #_(for [kv flock]
      (update-velocity flock (get kv :id))))

(comment
  (update-flock
   [{:id 1 :position [50 50] :velocity [0 0]}
    {:id 2 :position [70 70] :velocity [0 0]}
    {:id 3 :position [20 20] :velocity [0 0]}]))

(defn init-flock [W H N]
  (vec (for [i (range N)]
         {:id (inc i)
          :position (vector (rand-int W) (rand-int H))
          :velocity (normalize [(* 10 (- (rand) 0.5)) (* 10 (- (rand) 0.5))])})))

(defn tick-birb [flock birb]
  (let [MAXD 2e3
        {:keys [id position velocity]} birb
        N (count flock)
        cmass (->> flock (map :position) (centroid))
        vmass (mapv #(* % 0.5) (normalize (map - cmass position)))
        ;vmass [0 0]
        near (->> flock (remove #(or (= (:id %) id) (> (dist-sq (:position %) position) MAXD))))
        vrepel (normalize (->> near (map :position) (map #(map - position %)) (reduce #(mapv + %1 %2) [0 0])))
        vrand (normalize [(- (rand) 0.5) (- (rand) 0.5)])]
    {:id id :position (mapv + position vmass vrepel velocity) :velocity (mapv + vmass vrepel)}))

(defn tick-birb2 [flock cmass birb]
  (let [MAXD 5e3
        {:keys [id position velocity]} birb
        N (count flock)
        cmass (->> flock (map :position) (centroid))
        vmass (mapv #(* % 0.5) (normalize (map - cmass position)))
        ;vmass [0 0]
        near (->> flock (remove #(or (= (:id %) id) (> (dist-sq (:position %) position) MAXD))))
        vrepel (normalize (->> near (map :position) (map #(map - position %)) (reduce #(mapv + %1 %2) [0 0])))
        vrand (normalize [(- (rand) 0.5) (- (rand) 0.5)])]
    {:id id :position (mapv + position vmass vrepel velocity ) :velocity (mapv + velocity vmass vrepel)}))

(centroid [[0 0] [1 1]])
(comment
  (let [flock (init-flock 500 500 50)]
    (tick-birb flock (first flock))))

(defn tick-flock2 [flock]
  (let [cmass (->> flock (map :position) (centroid))]
    (mapv (fn [birb] (tick-birb2 flock cmass birb)) flock)))

(defn tick-flock [flock]
  (let [N (count flock)
        centroid
        (->> flock
             (map :position)
             (reduce (fn [[lx ly] [rx ry]] [(+ lx rx) (+ ly ry)]) [0 0])
             (mapv #(/ % N)))]
    (->> flock
         (map (fn [kv] (assoc kv :new-v (mapv + (:velocity kv) (normalize (mapv - centroid (:position kv)))))))
         (map (fn [kv] (assoc kv :position (mapv + (:position kv) (:new-v kv)) :velocity (:new-v kv)))))))

(comment
  (tick-flock (init-flock 50 50 10))
  (tick-flock2 (init-flock 50 50 10))
  (update-flock (vec (init-flock 800 800 100))))

(def foo (atom (init-flock 800 800 100)))

(defn draw [n-birbs]
  (let [flock (atom (init-flock 800 800 n-birbs))]
    (q/sketch
     :setup (fn [] (q/frame-rate 20))
     :size [800 800]
     :draw
     (fn []
       (let [g (q/create-graphics 800 800)]
         (q/with-graphics g
           (q/clear)
           (q/background 255 255)
           (q/stroke 0 100)
           (q/fill 0 100)
           (doseq [birb @flock]
             (q/ellipse (get (:position birb) 0) (get (:position birb) 1) 10 10)))
         (q/image g 0 0 800 800))
       (swap! flock tick-flock2)))))

(comment
  (draw 100))

(defn tst []
  (let [state (atom (init-flock 800 800 100))]
    (q/sketch
     :size [800 800]
     :setup (fn [] (q/frame-rate 15))
     :draw (fn []
             (let [G (q/create-graphics 800 800)]
               (q/with-graphics G
                 (q/clear)
                 (q/background 250 255)
                 (q/fill 0) (q/stroke 0)
                 (doseq [p @state] (q/ellipse (get p 0) (get p 1) 5 5)))
               (q/image G 0 0)
               (swap! state (fn [pts] (map (fn [[px py]] [(inc px) (inc py)]) pts))))))))

(defn static-drw [flock iters]
  (q/background 250)
  (q/stroke-weight 2)
  (q/stroke 0)
  (q/color-mode :hsb 1 100 100 1.0)
  (let [N (count flock)]
    (loop [iter 1 state flock]
      (if (<= iter iters)
        (let [state' (tick-flock2 state)]
          (dotimes [i N]
            ;(q/stroke (/ iter iters) 100 100)
            (let [[x y]   (:position   (get state i))
                  [x' y'] (:position   (get state' i))]
              (when (= iter 1) (q/ellipse x y 10 10))
              (q/line x y x' y')))
          (recur (inc iter) state'))
        nil))))

(declare birbs)
(q/defsketch birbs
  :setup (fn [] (q/no-loop))
  :settings q/smooth
  :size [800 800]
  :draw #(static-drw (init-flock 800 800 100) 35))

