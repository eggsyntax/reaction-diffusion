(ns reactiondiffusion.display
  (:require [quil.core :as q]))

(def w 200)
(def h 100)

(defn setup []
  (q/frame-rate 50)
  (q/background 30))

(defn draw []
  (q/stroke (q/random 255))
  (q/fill (q/random 255))
  (let [diam (q/random 5)
        x (q/random w)
        y (q/random h)]
    (q/ellipse x y diam  diam)))

(defn run []
  (q/defsketch example
    :title "foobar"
    :settings #(q/smooth 2)
    :renderer :opengl
    :setup setup
    :draw draw
    :size [w h]))
