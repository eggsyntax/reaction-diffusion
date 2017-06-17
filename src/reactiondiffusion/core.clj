(ns reactiondiffusion.core
  ;; use x for core.matrix; m as the convention for a matrix argument
  (:require [clojure.core.matrix :as x]
            #_[reactiondiffusion.display :as dis])
  (:gen-class))

;; TODO consider changing default implementation for performance gains:
;; See clojure.core.matrix.implementations/KNOWN-IMPLEMENTATIONS. Try 1st:
;; (clojure.core.matrix.implementations/set-current-implementation :vectorz)

;; Basic equation:
;;   a' = a     + (da * ave-diff)  ; diffusion term
;;              - (a * b * b)      ; subtracted by reaction
;;              + (fr * (1 - a))   ; feed

;;   b' = b     + (db * ave-diff)  ; diffusion term
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
(def w 200)
(def h 100)

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
         (x/emap-indexed (fn [[j i] v]
                           ;; (println "[i, j]" i j)
                           (let [x-diff (* -1 (Math/abs (- x-mid i)))
                                 y-diff (* -1 (Math/abs (- y-mid j)))
                                 x-val  (/ (+ x-mid x-diff) x-mid)
                                 y-val  (/ (+ y-mid y-diff) y-mid)]
                             (* x-val y-val)))))))

(defn init-middle-more []
  (let [x-mid (float (/ w 2))
        y-mid (float (/ h 2))]
    (->> (x/zero-matrix h w)
         (x/emap-indexed (fn [[j i] v]
                           ;; (println "[i, j]" i j)
                           (let [x-diff (* -1 (Math/abs (- x-mid i)))
                                 y-diff (* -1 (Math/abs (- y-mid j)))
                                 x-val  (/ (+ x-mid x-diff) w)
                                 y-val  (/ (+ y-mid y-diff) h)]
                             (+ x-val y-val)))))))

;; Set to any of the init- fns above
(def init init-middle-more)

;; display method for repl
(defn display-ascii [m]
  (x/pm (x/emap (fn [v] (condp <= v ; note: counterintuitively `<` because pred is called as (pred test-val v)
                          0.75 "#"
                          0.50 "+"
                          0.25 "."
                          0.00 " ")
                  #_(if (> v 0.5) "#" " "))
                m)))

;; display method for repl
(defn display-num
  "Convert numbers to single-point precision for easy display"
  [m]
  (x/pm (x/emap (fn [v] (format "%.1f" v))
                m)))

(defn val-at [m x y]
  ((m y) x))

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
    [y-start y-len x-start x-len])) ; reversed because matrix ops are in y x order

(defn surrounding-vals
  "Get the submatrix immediately surrounding the current position"
  [m x y]
  (apply x/submatrix m (index-ranges x y)))

(defn surrounding-ave
  "Get the average value surrounding a particular cell"
  [m x y]
  (let [neighborhood (surrounding-vals m x y)
        area-sum (x/esum neighborhood)]
    (/ (- area-sum (val-at m x y)) (- (x/ecount neighborhood) 1))))

;; End:   surrounding-average code

(defn ave-diff
  "The 2d Laplacian term representing diffusion"
  [m x y]
  (- (surrounding-ave m x y) (val-at m x y)))


(defn diffuse
  "Apply diffusion, at some rate, to this cell. Requires extra info (matrix,
  x, y) because it needs to get the average of surrounding cells in the matrix."
  [m rate [y x] v] ; v is value of cell
  (* rate (ave-diff m x y)))

(defn react
  "For sign, pass 1 to add the reaction amount or -1 to subtract it"
  [sign [x y] v]
  (* sign 0)
  )

(defn feed
  [[x y] v]
  0
  )

(defn kill
  [[x y] v]
  0
  )

;; No longer used, replaced by step-a/step-b
;; (defn diffuse-all [m rate]
;;   "Apply diffusion, at some rate, to all cells in matrix m"
;;   (println "advancing state.")
;;   (x/emap-indexed (fn [[y x] v] (+ v (diffuse m rate [y x] v))) m))

;; Original diffuse-all fn, called in the recur step of `run`:
;; (defn next-state [m]
;;   (m 6)
;;   (println "advancing state.")
;;   (x/emap-indexed (fn [[y x] v]
;;                     (+ v (ave-diff m x y)))
;;                   m))

;; TODO Alternate strategy for possible perf gains:
;; - Simple: could update a and b on separate threads.
;; - More complex: for each of a and b, could create a separate modification
;; step for each of react/diffuse/feed-or-kill. Each could run on a separate
;; thread, & then just add all the vectors to the current-val vector. Suspect
;; that's probably better. Code would likely be a
;; bit clearer too. For heaven's sake do it on a separate branch.

(defn step-a
  ;;   a' = a     + (da * ave-diff)  ; diffusion term
  ;;              - (a * b * b)      ; subtracted by reaction
  ;;              + (fr * (1 - a))   ; feed
  [m]
  (let [diffuse' (partial diffuse m da) ; diffuse gets extra info
        react' (partial react -1)
        ;; each step-fn (feed, reach, diffuse') has signature [[y x] v]
        step-ops (juxt feed react' diffuse')
        step-fn (fn [[y x] v] (apply + v (step-ops [y x] v)))]
    (x/emap-indexed step-fn m)))

(defn step-b
  ;;   b' = b     + (db * ave-diff)  ; diffusion term
  ;;              + (a * b * b)      ; added by reaction
  ;;              - ((kr + fr) * b)  ; kill
  [n]
  (let [diffuse' (partial diffuse n db) ; diffuse gets extra info
        react' (partial react 1)
        ;; each step-fn (kill, reach, diffuse') has signature [[y x] v]
        step-ops (juxt kill react' diffuse')
        step-fn (fn [[y x] v] (apply + v (step-ops [y x] v)))]
    (x/emap-indexed step-fn n)))

(defn run []
  (let [m (init)
        n (init)]
    (loop [mp m
           np n
           i 30]
      (display-num np)
      (display-ascii np)
      (println)
      ;; (Thread/sleep 30)
      (when (>= i 0)
        (recur (step-a mp da) (step-b np db) (- i 1))))))
