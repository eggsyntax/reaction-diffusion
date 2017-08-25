(ns reactiondiffusion.display
  (:require [quil.core :as q]
            [clojure.core.async :as a :refer [<! go]]
            [clojure.core.matrix :as x]
            [reactiondiffusion.core :as core]))

(def cell-size 5)

(def w (* core/w cell-size))
(def h (* core/h cell-size))

(def cell-w (int (/ w core/w)))
(def cell-h (int (/ h core/h)))

;; (def a-state (atom (core/init-middle)))
(def a-state (atom nil))
(def b-state (atom nil))

(defn vcolor [a b]
  [(* a 255) 40 (* b 255)])

;; smooth a:green, b:blue
;; (defn vcolor [a b]
;;   [80 (* a 255) (* b 255)])

;; show greater
;; (defn vcolor [a b]
;;   (cond (> a (+ b 0.05)) [255 255 255]
;;         (> b (+ a 0.05)) [  0   0   0]
;;         :else            [127   0   0]))

;; ;; show greater with fades
;; (defn vcolor [a b]
;;   (cond (> a (+ b 0.01)) [ 30 (* a 255)       0]
;;         (> b (+ a 0.01)) [ 30        0 (* b 255)]
;;         :else            [180       90       90]))

;; (defn vcolor [a b]
;;   ;; [(* a 255) (* b 255) 40]
;;   (if (> a b)
;;     [220 240 230]
;;     [10 15 30])
;;   )

;; (defn vcolor [a b]
;;   (let [diff (- a b)] ; -1..1
;;     (condp < diff
;;       0.0  [220 240 230]
;;       -0.5 [120 140 130]
;;       ,    [ 10  15  30])))

(defn setup []
  (x/set-current-implementation :vectorz)
  (q/frame-rate 60)
  (q/background 0)
  (reset! a-state (core/init-one))
  ;; (reset! b-state (core/init-seed-in-middle 15 0.6))
  (reset! b-state (core/init-middle))
  )

(defn draw-cell [[y x] _]
  ;; (print x y a " -  ")
  (let [a (x/mget @a-state y x)
        b (x/mget @b-state y x)]
    (apply q/stroke (vcolor a b))
    (apply q/fill (vcolor a b))
    (q/rect (* cell-w x) (* cell-h y) cell-w cell-h)))

(defn draw []
  (q/background 0) ; clear screen
  ;; (println "@a-state:" @a-state)
  ;; (println "@b-state:" @b-state)
  ;; (do (x/emap-indexed draw-cell @a-state)) ; note we ignore state arg; we just need it for the size
  ;; (print "drawing: ")
  (try

    ;; TODO temp
    ;; nil
    (x/emap-indexed draw-cell @a-state) ; note we ignore state arg; we just need it for the size
    (catch NullPointerException e
      #_(println "Caught NPE" (.getMessage e))
      ))
  ;; (println "done drawing.")
  (let [mx (q/mouse-x)
        my (q/mouse-y)
        xf (/ mx w)
        yf (/ my h)
        x (int (* xf core/w))
        y (int (* yf core/h))
        oldaval (x/mget @a-state y x)
        oldbval (x/mget @b-state y x)
        oldabdiff (- (x/mget @a-state y x) (x/mget @b-state y x))]
    (reset! a-state (core/step-a @a-state @b-state))
    (reset! b-state (core/step-b @b-state @a-state))
    ;; DEBUG PRINT
    (let [aval (x/mget @a-state y x)
          bval (x/mget @b-state y x)
          adiff (- aval oldaval)
          bdiff (- bval oldbval)
          abdiff (- aval bval)]
      (println (format "aval@mouse: %.12f  %.12f bval: %.12f  %.12f;   %.12f  %.12f"
                       aval
                       adiff
                       bval
                       bdiff
                       abdiff
                       (- abdiff oldabdiff))))))

(defn run []
  (q/defsketch example
    :size [w h]
    :title "foobar"
    :settings #(q/smooth 2)
    :renderer :opengl
    :setup setup
    :draw draw))
