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
(def w 15)
(def h 15)
;; Equation constants
(def da 1.0)   ; diffusion rate of a
(def db 0.5)   ; diffusion rate of b
(def fr 0.055) ; feed rate of a
(def kr 0.062) ; kill rate of b

(defn -main
  "I don't do a whole lot ... yet."
  [& args])

(defn init-rand []
  (->> (x/zero-matrix h w)
       (x/emap (fn [_] (rand)))))

(defn init-middle []
  (let [x-mid (float (/ w 2))
        y-mid (float (/ h 2))]
    (->> (x/zero-matrix h w)
         (x/emap-indexed (fn [[i j] v]
                           ;; (println "[i, j]" i j)
                           (let [x-diff (* -1 (Math/abs (- x-mid i)))
                                 y-diff (* -1 (Math/abs (- y-mid j)))
                                 x-val  (/ (+ x-mid x-diff) x-mid)
                                 y-val  (/ (+ y-mid y-diff) y-mid)]
                             (* x-val y-val)))))))

(def init init-middle)

(defn display-ascii [m]
  (x/pm (x/emap (fn [v] (condp <= v ; note: counterintuitively `<` because pred is called as (pred test-val v)
                         0.66 "#"
                         0.33 "+"
                         0.00 " ")
                  #_(if (> v 0.5) "#" " "))
                m)))

(defn display-num [m]
  (x/pm (x/emap (fn [v] (format "%.1f" v))
                m)))

;; Begin: surrounding-average code
(defn capped-val [v cap]
  (min (max v 0) (- cap 1)))

(defn index-ranges
  "Return [start length] pairs for x and y, limited to not go out-of-bounds"
  ;; Necessary because submatrix throws exception if any of submatrix is out-of-bounds.
  ;; Would be much nicer for my purposes if submatrix just got a capped submatrix.
  [x y]
  ;; (assert (and (> x 0) (< x (- w 1)) (> y 0) (< y (- h 1))))
  (let [x-start (capped-val (- x 1) w)
        x-end   (capped-val (+ x 1) w)
        x-len   (- x-end x-start -1)
        y-start (capped-val (- y 1) h)
        y-end   (capped-val (+ y 1) h)
        y-len   (- y-end y-start -1)]
    [y-start y-len x-start x-len])) ; reversed because mat ops in y x order

(defn surrounding-vals
  "Get the submatrix immediately surrounding the current position"
  [m x y]
  (apply x/submatrix m (index-ranges x y)))

(defn surrounding-ave
  "Get the average value surrounding a particular cell"
  [m x y]
  (let [curval ((m y) x)
        neighborhood (surrounding-vals m x y)
        area-sum (x/esum neighborhood)]
    (/ (- area-sum curval) (- (x/ecount neighborhood) 1))))

;; End:   surrounding-average code
