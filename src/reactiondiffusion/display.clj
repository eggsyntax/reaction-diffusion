(ns reactiondiffusion.display
  (:require [quil.core :as q]
            [clojure.core.async :as a :refer [<! go]]
            [clojure.core.matrix :as x]
            [reactiondiffusion.core :as core]))

(def w 800)
(def h 400)

(def cell-w (int (/ w core/w)))
(def cell-h (int (/ h core/h)))

;; (def a-state (atom (core/init-middle)))
(def a-state (atom nil))
(def b-state (atom nil))

;; (defn vcolor [a b]
;;   [(* a 255) (* b 255) 40])

(defn vcolor [a b]
  ;; [(* a 255) (* b 255) 40]
  (if (> a b)
    [220 240 230]
    [10 15 30])
  )

(defn setup []
  (x/set-current-implementation :vectorz)
  (q/frame-rate 40)
  (q/background 0)
  (reset! a-state (core/init-one))
  (reset! b-state (core/init-rand)))

(defn draw-cell [[y x] _]
  ;; (print x y a " -  ")
  (let [a (x/mget @a-state y x)
        b (x/mget @b-state y x)]
    (apply q/stroke (vcolor a b))
    (apply q/fill (vcolor a b))
    (q/rect (* cell-w x) (* cell-h y) cell-w cell-h)
    ;; (print x y a b " :")
    ))

(defn draw []
  (q/background 0) ; clear screen
  ;; (println "@a-state:" @a-state)
  ;; (println "@b-state:" @b-state)
  ;; (do (x/emap-indexed draw-cell @a-state)) ; note we ignore state arg; we just need it for the size
  (print "drawing: ")
  (time
   (try
     (x/emap-indexed draw-cell @a-state) ; note we ignore state arg; we just need it for the size
     (catch NullPointerException e
       (println "Caught NPE" (.getMessage e)))))
  (println "done drawing.")
  ;; (println)
  (reset! a-state (core/step-a @a-state @b-state))
  (reset! b-state (core/step-b @b-state @a-state)))

(defn run []
  (q/defsketch example
    :size [w h]
    :title "foobar"
    :settings #(q/smooth 2)
    :renderer :opengl
    :setup setup
    :draw draw))
