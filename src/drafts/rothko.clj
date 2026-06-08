;;TEMU ROTHKO
(ns drafts.rothko
  (:require [quil.core :as q]
            [util.misc]
            [util.masking :refer [mask-w-alpha]]))
            
(declare sketch)

(defn draw-shape
  [vxs]
  (q/begin-shape)
  (doseq [vx vxs]
    (apply q/vertex vx))
  (q/end-shape))

(defn rect [[nex ney] width height w-segments h-segments]
  (let [top (mapv #(vector % ney) (range nex (+ nex width w-segments) w-segments))
        right (mapv #(vector (+ nex width) %) (range ney (+ ney height h-segments) h-segments))
        bottom (mapv #(vector % (+ ney height)) (reverse (range nex (+ nex width w-segments) w-segments)))
        left (mapv #(vector nex %) (reverse (range ney (+ ney height h-segments) h-segments)))]
    (distinct (concat top right bottom left))))

(defn textured-shape [layers color shape-fn]
  (let [g1 (q/create-graphics (q/width) (q/height))
        g2 (q/create-graphics (q/width) (q/height))]
    (dotimes [_ layers]
      (q/with-graphics g1
        (q/clear)
        (q/no-stroke)
        (q/fill 0)
        (draw-shape (shape-fn)))
      (q/with-graphics g2
        (q/clear)
        ;(q/color-mode :hsb 359 100 100 1.0)
        (q/color-mode :rgb 255 255 255 1.0)
        (q/no-stroke)
        (apply q/fill (conj color 0.04))
        (dotimes [_ 100]
          (q/ellipse (rand-int (q/width)) (rand-int (q/height)) 70 90)))
      (mask-w-alpha g2 g1)
      (q/image g2 0 0))))
  
(defn draw []
  (q/color-mode :hsb 359 100 100 100)
  (q/no-stroke)
  (q/rect-mode :center)
  (dotimes [_ 2000]
    (q/fill 0 (+ 80 (* 10 (rand))) 100 4)
    (q/rect (rand-int (q/width)) (rand-int (q/height)) (rand-int (/ (q/width) 3)) (rand-int (/ (q/height) 3))))
  (let [W 300 H 100 xstart 50 ystart 300]
    (dotimes [_ 2000]
      (q/fill 0 (+ 20 (* 10 (rand))) (+ 50 (* 5 (rand))) 20)
      (q/rect (+ xstart (rand-int  W)) (rand-int (+ ystart (rand-int H)))
              (rand-int (max (/ (- (+ xstart (rand-int  W)) xstart) 2) (/ (- (- W xstart) 2) (+ xstart (rand-int  W))) (/ W 3)))
              (rand-int (max (/ (- (+ ystart (rand-int  H)) xstart) 2) (/ (- (- H xstart) 2) (+ ystart (rand-int  H))) (/ H 3)))))))
        
(q/defsketch sketch
  :setup (fn [] (q/no-loop))
  :size [400 800]
  :draw draw)
