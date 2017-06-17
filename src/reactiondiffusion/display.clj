(ns reactiondiffusion.display
  (:require [quil.core :as q]
            [clojure.core.async :as a :refer [<! go]]
            [clojure.core.matrix :as x]
            [reactiondiffusion.core :as core]))

(def w 800)
(def h 400)

;; (def state (atom (core/init-middle)))
(def state (atom nil))

(defn vcolor [v]
  []
  [(* v 255) (* v 140) 50])

(defn setup []
  (q/frame-rate 40)
  (q/background 0)
  (reset! state (core/init)))

(defn draw-cell [[y x] v]
  ;; (print x y v " -  ")
  (apply q/stroke (vcolor v))
  (apply q/fill (vcolor v))
  (q/rect (* 4 x) (* 4 y) 4 4))

(defn draw []
  (println "drawing.")
  (q/background 0) ; clear screen
  (do (x/emap-indexed draw-cell @state))
  (println)
  (reset! state (core/diffuse-all @state core/da)))

(defn run []
  (q/defsketch example
    :size [w h]
    :title "foobar"
    :settings #(q/smooth 2)
    :renderer :opengl
    :setup setup
    :draw draw))
