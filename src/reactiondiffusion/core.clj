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
(def w 400)
(def h 200)

(def x-mid (int (/ w 2)))
(def y-mid (int (/ h 2)))

;; Equation constants
(def da 0.9)   ; diffusion rate of a
(def db 0.4)   ; diffusion rate of b
(def fr 0.050) ; feed rate of a
(def kr 0.060) ; kill rate of b

(defn -main
  "I don't do a whole lot ... yet."
  [& args])

(defn init-one []
  (->> (x/zero-matrix h w)
       (x/emap (fn [_] 0.98))))

(defn init-zero []
  (x/zero-matrix h w))

(defn init-rand []
  (->> (x/zero-matrix h w)
       (x/emap (fn [_] (/ (rand) 0.97)))))

(defn init-middle []
  (->> (x/zero-matrix h w)
       (x/emap-indexed (fn [[j i] v]
                         ;; (println "[i, j]" i j)
                         (let [x-diff (* -1 (Math/abs (- x-mid i)))
                               y-diff (* -1 (Math/abs (- y-mid j)))
                               x-val  (/ (+ x-mid x-diff) (+ 1 x-mid))
                               y-val  (/ (+ y-mid y-diff) (+ 1 y-mid))]
                           (* x-val y-val))))))

(defn init-middle-more []
  (let [x-mid (float (/ w 2))
        y-mid (float (/ h 2))]
    (->> (x/zero-matrix h w)
         (x/emap-indexed (fn [[j i] v]
                           ;; (println "[i, j]" i j)
                           ;; x-diff, y-diff -- x or y distance from center
                           (let [x-diff (* -1 (Math/abs (- x-mid i)))
                                 y-diff (* -1 (Math/abs (- y-mid j)))
                                 x-val  (/ (+ x-mid x-diff) (+ 1 w))
                                 y-val  (/ (+ y-mid y-diff) (+ 1 h))]
                             (+ x-val y-val)))))))

;; Set to any of the init- fns above
(def init init-middle-more)

;; display method for repl
(defn display-ascii [m]
  #_(x/pm (x/emap (fn [v] (do
                           (condp <= v ; note: counterintuitively `<` because pred is called as (pred test-val v)
                             1.00 "!" ; shouldn't see these
                             0.75 "#"
                             0.50 "+"
                             0.25 "."
                             -0.00 " "
                             -10.00 "&" ; shouldn't see these
                             )))
                  m))
  (run! println
        (map
         #(map (fn [v]
                 (do
                   #_(println v (type v))
                   (condp <= v ; note: counterintuitively `<` because pred is called as (pred test-val v)
                     1.00 "!" ; shouldn't see these
                     0.75 "#"
                     0.50 "+"
                     0.25 "."
                     -0.00 " "
                     -10.00 "&" ; shouldn't see these
                     )))
               %)
         m)))

;; display method for repl
(defn display-num
  "Convert numbers to single-point precision for easy display"
  [m]
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
    [y-start y-len x-start x-len])) ; reversed because matrix ops are in y x order

(defn get-neighborhood
  "Get the submatrix immediately surrounding the current position"
  [m x y]
  (apply x/submatrix m (index-ranges x y)))

;; (defn weight-neighborhood [neighborhood]
;;   ;; Assumes neighborhood is 3x3
;;   (letfn [(weight [[y x] v]
;;             (cond
;;               (and (= x 1) (= y 1)) 0      ; don't include self
;;               (or  (= x 1) (= y 1)) (* 0.2 v)      ; orthogonal
;;               :else                 (* 0.05 v)))]))  ; diagonal

(defn weight-neighborhood [[y x] v]
  ;; Assumes neighborhood is 3x3
  ;; TODO hence gives wrong value at edges.
  (cond
    (and (= x 1) (= y 1)) 0      ; don't include self
    (or  (= x 1) (= y 1)) (* 0.2 v)      ; orthogonal
    :else                 (* 0.05 v)))   ; diagonal

(defn surrounding-ave
  "Get the average value surrounding a particular cell"
  [m x y]
  (let [neighborhood (get-neighborhood m x y)
        weighted-n (x/emap-indexed weight-neighborhood neighborhood)
        area-sum (x/esum weighted-n)]
    ;; weighting forces divisor of one, so skip dividing.
    area-sum))

;; End:   surrounding-average code

(defn ave-diff
  "The 2d Laplacian term representing diffusion"
  [m x y]
  (- (surrounding-ave m x y) (x/mget m y x)))

(def in-a (atom nil)) ; needed when printing debug output

(defn diffuse
  "Apply diffusion, at some rate, to this cell. Requires extra info (matrix,
  x, y) because it needs to get the average of surrounding cells in the matrix."
  [m rate [y x] v] ; v is value of cell
  (let [res (* rate (ave-diff m x y))]
    (when (and (= y y-mid) (= x x-mid) @in-a)
      #_(println "val diffuse" res))
    res)
  ;; (* rate (ave-diff m x y))
)

(defn react
  "For sign, pass 1 to add the reaction amount or -1 to subtract it. For other,
  pass another matrix to react with."
  [sign other [y x] v]
  (let [other-cur (x/mget other y x)]
    ;; (println (format "%2d %8f %8f %8f" sign v other-cur (* sign v other-cur other-cur)))
    ;; (let [res (* sign v other-cur other-cur)]
    ;;   (when (and (= y y-mid) (= x x-mid) @in-a)
    ;;     #_(println "other-cur" other-cur)
    ;;     #_(println "val react  " res))
    ;;   res)
    (* sign v other-cur other-cur)
))

(defn feed
  [[y x] v]
  ;; (* fr (- 1 v))
  ;; print a representative value
  (let [res (* fr (- 1 v))]
    (when (and (= y y-mid) (= x x-mid) @in-a)
      #_(println "val feed   " res))
    res))

(defn kill
  [[y x] v]
  (let [res (* -1 (+ kr fr) v)]
    (when (and (= y y-mid) (= x x-mid) @in-a)
      #_(println "val kill   " res))
    res)
  ;; (* -1 (+ kr fr) v)
)

;; TODO Concurrency strategies for possible perf gains:
;; - Simple: could update a and b on separate threads.
;; - Medium: use reducers to parallelize the map-indexed
;; - More complex: for each of a and b, could create a separate modification
;; step for each of react/diffuse/feed-or-kill. Each could run on a separate
;; thread, & then just add all the vectors to the current-val vector. Suspect
;; that's probably better. Code would likely be a
;; bit clearer too. For heaven's sake do it on a separate branch.

(defn step-a
  ;;   a' = a     + (da * ave-diff)  ; diffusion term
  ;;              - (a * b * b)      ; subtracted by reaction
  ;;              + (fr * (1 - a))   ; feed
  [m other]
  (reset! in-a true)
  (let [diffuse' (partial diffuse m da) ; diffuse gets extra info
        react' (partial react -1 other)
        ;; each step-fn (feed, reach, diffuse') has signature [[y x] v]
        step-ops (juxt feed react' diffuse')
        step-fn (fn [[y x] v] #_(apply + v (step-ops [y x] v))
                  (let [res (max -0.995 (min 0.995 (apply + v (step-ops [y x] v))))]
                    (when (and (= y y-mid) (= x x-mid) @in-a)
                      #_(println "       cur" v)
                      #_(println "   new-val" res))
                    res))]
    (println "step-a")
    (time (x/emap-indexed step-fn m))))

(defn step-b
  ;;   b' = b     + (db * ave-diff)  ; diffusion term
  ;;              + (a * b * b)      ; added by reaction
  ;;              - ((kr + fr) * b)  ; kill
  [n other]
  (reset! in-a nil)
  (let [diffuse' (partial diffuse n db) ; diffuse gets extra info
        react' (partial react 1 other)
        ;; each step-fn (kill, reach, diffuse') has signature [[y x] v]
        step-ops (juxt kill react' diffuse')
        step-fn (fn [[y x] v] (max -0.995 (min 0.995 (apply + v (step-ops [y x] v)))))]
    (x/emap-indexed step-fn n)))

(defn run []
  ;; For running in repl -- for Quil display, call display/run
  (x/set-current-implementation :vectorz)
  (let [m (init)
        n (init-rand)]
    (loop [mp m
           np n
           i 30]
      ;; (display-num mp)
      (display-ascii mp)
      ;; (display-ascii np)
      #_(println)
      ;; (Thread/sleep 30)
      (when (>= i 0)
        (recur (step-a mp np) (step-b np mp) (- i 1))))))
