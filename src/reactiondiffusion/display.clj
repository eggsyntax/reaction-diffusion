(ns reactiondiffusion.display
  (:require [quil.core :as q]
            [clojure.core.async :as a :refer [<! go]]
            [clojure.core.matrix :as x]
            [reactiondiffusion.core :as core]))

(def w 200)
(def h 100)

(def state (atom (core/init-middle))) ; updated by core

(defn setup []
  (q/frame-rate 5)
  (q/background 30))

;; (defn draw-cell [m x y]
;;   (let [v ((m y) x)]
;;     (q/fill (* v 255) 30 50)
;;     (q/rect x y 1 1)))

;; TODO not used yet
;; Assume that draw-cell will be called from x/map-indexed
(defn draw-cell [[y x] v]
  ;; (println x y v)
  (q/fill (* v 255) 30 50)
  (q/rect x y 1 1))

;; TODO draw can't (I think!) take params.
;; Is there a way to? Assuming not, could:
;; - Create an atom for m which is updated by core/next-state
;; - Could pass in current state map all through the fns in this ns
;; - Global vars to hold everything?
(defn draw []
  ;; (q/stroke (q/random 255))

  (let [diam (q/random 5)
        x (q/random w)
        y (q/random h)]
    (println "drawing.")
    (x/emap-indexed draw-cell @state)
    (reset! state (core/next-state @state))

    ))

(defn run []
  (q/defsketch example
    :size [w h]
    :title "foobar"
    :settings #(q/smooth 2)
    :renderer :opengl
    :setup setup
    :draw draw))
