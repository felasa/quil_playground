(ns util.core)

(defn gradient [min-x max-x x]
  (/ (- x min-x) max-x))
   
(defn gradient-2
  [p-ini p-end p]
  (let [x-ini (get p-ini 0) y-ini (get p-ini 1)
        x-end (get p-end 0) y-end (get p-end 0)
        b (/ (- y-end y-ini) (- x-end x-ini))]
    []))

(defn coord-from-idx
  "Map unidimensional index to two-dimensional coordinate of given width"
  [width idx]
  [(mod idx width)
   (quot idx width)])

(defn idx-from-coord
  "Map bi-dimensional coordinate from space of given width to one-dimensional index"
  [width [x y]]
  (+ x (* y width)))

(defn norm
  "L2 norm (euclidian lenght) of point p"
  [p]
  (Math/sqrt (reduce + (map #(* % %) p))))

(defn dist 
  "Euclidian distance between p1 and p2"
  [p1 p2]
  (norm (mapv - p1 p2)))
  
(defn vector-add
  [p1 p2]
  (mapv + p1 p2))

(defn dot [p1 p2]
  (mapv * p1 p2))

(defn poly-contains-p?
  [poly p]
  (let [xs (mapv #(get % 0) poly)
        ys (mapv #(get % 1) poly)]
    (loop [i 0 j (dec (count poly)) ret false]
      (if (< i (count poly)) 
        (let [deltax (- (xs j) (xs i))
              yspread (- (p 1) (ys i))
              deltay (- (ys j) (ys i))]
          (if (and (not= (> (ys i) (p 1)) (> (ys j) (p 1)))
                   (< (p 0)
                      (+ (/ (* deltax yspread) deltay) (xs i))))
            (recur (inc i) i (not ret))
            (recur (inc i) i ret)))
        ret))))

(defn polys-overlap? [poly1 poly2]
  (or
    (loop [rem poly2]
      (if-let [p (peek rem)]
        (if (poly-contains-p? poly1 p) true
          (recur (pop rem)))
        false))
    (loop [rem poly1]
      (if-let [p (peek rem)]
        (if (poly-contains-p? poly2 p) true
          (recur (pop rem)))
        false))))

