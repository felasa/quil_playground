(ns core
  (:require [quil.core :as q]
            [dynamic :as dynamic])
  (:gen-class))

(declare example)
(q/defsketch example
             :title "Sketch"
             :setup dynamic/setup
             :draw dynamic/draw
             ; :renderer :p2d
             :size [900 900])

(defn refresh []
  (use :reload 'sketch.dynamic)
  (.redraw example))

(defn get-applet []
  example)
