(ns reactiondiffusion.core
  ;; use x for core.matrix; m as the convention for a matrix argument
  (:require [clojure.core.matrix :as x])
  (:gen-class))

;; Basic equation:
;;   a' = a     + (da * avediff)   ; diffusion term
;;              - (a * b * b)      ; subtracted by reaction
;;              + (fr * (1 - a))   ; feed

;;   b' = b     + (db * avediff)   ; diffusion term
;;              + (a * b * b)      ; added by reaction
;;              - ((kr + fr) * b)  ; kill

;; useful core.matrix fns
;; x/emap - apply a fn element-wise to a matrix
;; x/esum - return sum of all elements
;; x/pm - pretty-print matrix
;; x/submatrix - to use for convolution

;; Constants
;; Display constants
(def w 3)
(def h 3)
;; Equation constants
(def da 1.0)   ; diffusion rate of a
(def db 0.5)   ; diffusion rate of b
(def fr 0.055) ; feed rate of a
(def kr 0.062) ; kill rate of b

(defn -main
  "I don't do a whole lot ... yet."
  [& args])

(defn init []
  (->> (x/zero-matrix h w)
       (x/emap (fn [_] (rand)))))

(defn display [m]
  (x/pm (x/emap (fn [x] (if (> x 0.5) "#" " "))
                (x/coerce [] m)))
  ;; (x/pm m)
  )
