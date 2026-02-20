(ns util.random
  (:require [quil.core :as q]
            [clojure.math :as math]))

(defn gauss
  [mu sigma]
  (+ mu (* sigma (q/random-gaussian))))

;; 'manual' simulations, probably better sticking with library ones
;; but it would be desirable to get methods from outside of processing lib
(defn draw-binormal 
  ([]
   (let [u1 (rand)
         u2 (rand)]
     [(* (math/sqrt (* -2 (math/log u1))) (math/cos (* 2 math/PI u2)))
      (* (math/sqrt (* -2 (math/log u1))) (math/sin (* 2 math/PI u2)))]))
  ([[xm ym :as mean]] 
   (let [Z (draw-binormal)]
     (mapv + mean Z)))
  ([mean [stdx stdy :as std]]
   (let [Z (draw-binormal)]
     (mapv + mean (map * std Z)))))

(defn rbinorm
  [times & params]
  (let [sample (transient (vector))]
    (dotimes [n times]
      (conj! sample (apply draw-binormal params)))
    (persistent! sample)))

(defn rnorm
  ([times]
   (let [sample (transient (vector))]
     (dotimes [n times]
       (conj! sample ((draw-binormal) 0)))
     (persistent! sample)))
  ([times mean std] (mapv #(+ mean %) (map #(* std %) (rnorm times)))))

(def grad3 [[1,1,0], [-1,1,0], [1,-1,0], [-1,-1,0],
            [1,0,1],[-1,0,1],[1,0,-1],[-1,0,-1],
            [0,1,1],[0,-1,1],[0,1,-1],[0,-1,-1]])

(def p
  [151,160,137,91,90,15, 131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,
   8,99,37,240,21,10,23,
   190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
   88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
   77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
   102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
   135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
   5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
   223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
   129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
   251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
   49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
   138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180])

(def perm 
  (mapv (fn [idx] (get p (bit-and idx 255)))
        (range 512)))

(defn simplex [x y]
  (let [p (into [] (shuffle (range 256)))
        F2 0.366025403 ;(* 0.5 (- (math/sqrt 3) 1))
        s (* F2 (+ x y))
        xs (+ x s) ys (+ y s)
        i (if (>= xs 0) (int xs) (int (- xs 1)))
        j (if (>= ys 0) (int ys) (int (- ys 1)))
        G2 0.211324865 ;(/ (- 3.0 (math/sqrt 3)) 6)
        t (* G2 (double (+ i j)))
        X0 (- i t) Y0 (- j t)
        x0 (- x X0) y0 (- y Y0)
        [i1 j1] (if (> x0 y0) [1 0] [0 1])
        x1 (+ G2 x0 (- i1)) y1 (+ G2 y0 (- j1))
        x2 (+ (* 2.0 G2) x0 (- 1)) y2 (+ (* 2.0 G2) y0 (- 1.0))
        ii (bit-and i 255) jj (bit-and j 255)
        gi0 (mod (get perm (+ ii (get perm jj))) 12) 
        gi1 (mod (get perm (+ ii i1 (get perm (+ jj j1)))) 12)
        gi2 (mod (get perm (+ ii (get perm (+ jj 1)))) 12)
        t0' (- 0.5 (* x0 x0) (* y0 y0)) 
        t0 (if (< t0' 0) t0' (* t0' t0'))
        n0 (if (< t0' 0) 0 (* t0 t0 (reduce + (mapv * (get grad3 gi0) [x0 y0]))))
        t1' (- 0.5 (* x1 x1) (* y1 y1))
        t1 (if (< t1' 0) t1' (* t1' t1'))
        n1 (if (< t1' 0) 0 (* t1 t1 (reduce + (mapv * (get grad3 gi1) [x1 y1]))))
        t2' (- 0.5 (* x2 x2) (* y2 y2))
        t2 (if (< t2' 0) t2' (* t2' t2'))
        n2 (if (< t2' 0) 0 (* t2 t2 (reduce + (mapv * (get grad3 gi2) [x2 y2]))))]
    (* 70.148058701579075 (+ n0 n1 n2))))

;(reduce min (for [i (range 1000)] (simplex (rand-int 800) (rand-int 800)))) ;; -0.014255562142184909
;;// Calculate the contribution from the three corners
;;double t0 = 0.5 - x0*x0-y0*y0;
;;if(t0<0) n0 = 0.0;
;;else {
;;      t0 *= t0;
;;      n0 = t0 * t0 * dot(grad3[gi0], x0, y0)};  // (x,y) of grad3 used for 2D gradient
;;    
;;    double t1 = 0.5 - x1*x1-y1*y1;
;;    if(t1<0) n1 = 0.0;
;;    else {
;;          t1 *= t1;
;;          n1 = t1 * t1 * dot(grad3[gi1], x1, y1)};
;;    
;;    double t2 = 0.5 - x2*x2-y2*y2;
;;    if(t2<0) n2 = 0.0;
;;    else {
;;          t2 *= t2;
;;          n2 = t2 * t2 * dot(grad3[gi2], x2, y2)};
;;    
;;    // Add contributions from each corner to get the final noise value.
;;    // The result is scaled to return values in the interval [-1,1].
;;    return 70.0 * (n0 + n1 + n2);
;;
