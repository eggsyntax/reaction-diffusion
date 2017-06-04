(ns reactiondiffusion.core
  ;; use x for core.matrix; m as the convention for a matrix argument
  (:require [clojure.core.matrix :as x])
  (:gen-class))

;; TODO consider changing default implementation for performance gains:
;; See clojure.core.matrix.implementations/KNOWN-IMPLEMENTATIONS. Try 1st:
;; (clojure.core.matrix.implementations/set-current-implementation :vectorz)

;; Basic equation:
;;   a' = a     + (da * avediff)   ; diffusion term
;;              - (a * b * b)      ; subtracted by reaction
;;              + (fr * (1 - a))   ; feed

;;   b' = b     + (db * avediff)   ; diffusion term
;;              + (a * b * b)      ; added by reaction
;;              - ((kr + fr) * b)  ; kill

;; Note that matrix operations are indexed [y x]

;; useful core.matrix fns
;; x/emap - apply a fn element-wise to a matrix
;; x/esum - return sum of all elements
;; x/pm - pretty-print matrix
;; x/submatrix - to use for convolution

;; Constants
;; Display constants
(def w 8)
(def h 8)
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

(defn display-ascii [m]
  (x/pm (x/emap (fn [v] (if (> v 0.5) "#" " "))
                (x/coerce [] m))))

(defn capped-val [v cap]
  ;; we have to subtract 2 from cap: 1 because we 0-index and 1 because
  ;; we don't want them right on the borders
  (min (max v 1) (- cap 2)))

(defn index-ranges
  "Return [start length] pairs for x and y, limited to not go out-of-bounds"
  ;; Necessary because submatrix throws exception if any of submatrix is out-of-bounds.
  ;; Would be much nicer for my purposes if submatrix just got a capped submatrix.
  [x y]
  (assert (and (> x 0) (< x (- w 1)) (> y 0) (< y (- h 1))))
  (let [x-start (capped-val (- x 1) w)
        x-end   (capped-val (+ x 1) w)
        x-len   (- x-end x-start -1)
        y-start (capped-val (- y 1) h)
        y-end   (capped-val (+ y 1) h)
        y-len   (- y-end y-start -1)]
    [x-start x-len y-start y-len]))

(defn surrounding-vals
  "Get the submatrix immediately surrounding the current position"
  [m x y]
  (apply x/submatrix m (index-ranges x y)))
