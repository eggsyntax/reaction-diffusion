(ns reactiondiffusion.display
  (:require [quil.core :as q]
            [clojure.core.async :as a :refer [<! go]]
            [clojure.core.matrix :as x]
            [reactiondiffusion.color :as color]
            [reactiondiffusion.core :as core]))

(def w (* core/w cell-size))
(def h (* core/h cell-size))

(def cell-w (int (/ w core/w)))
(def cell-h (int (/ h core/h)))

(def a-state (atom nil))
(def b-state (atom nil))

(def show-param-values (atom false))
(def show-values       (atom  true))

(defn get-params []
  (format "
a-diffusion: %.4f\n
b-diffusion: %.4f\n
feed rate:   %.4f\n
kill rate:   %.4f\n
react rate:  %.4f\n
time step:   %.4f\n"
          @core/da @core/db @core/fr @core/kr @core/rr @core/t))

(defn setup []
  (x/set-current-implementation :vectorz)
  (q/smooth)
  (q/text-font (q/create-font "Fira Mono Bold" 18 true))
  (q/frame-rate 60)
  (q/background 0)
  ;; (q/stroke-weight 20)
  (q/no-stroke)
  (reset! a-state (core/init-one))
  (reset! b-state (core/init-seed-in-middle 30 1.0))
  ;; (reset! b-state (core/init-middle))
  )

(defn draw-cell [[y x] _]
  ;; (print x y a " -  ")
  (let [a (x/mget @a-state y x)
        b (x/mget @b-state y x)
        color-fn (color/colors @color/cur-color)]
    ;; (apply q/stroke (color-fn a b))
    (apply q/fill   (color-fn a b))
    (q/rect (* cell-w x) (* cell-h y) cell-w cell-h)))

(defn draw []
  (q/background 0) ; clear screen
  (try
    (x/emap-indexed draw-cell @a-state) ; note we ignore state arg; we just need it for the size
    (catch NullPointerException e))
  (when @show-param-values
    (q/fill 200 0 140)
    (q/text (get-params) 20 20))
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
    (when @show-values
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
                         (- abdiff oldabdiff)))))))

(defn as-var [kwd]
  (deref (ns-resolve 'reactiondiffusion.core (symbol (name kwd)))))

(defn- increase
  "For documentation purposes, pass a keyword version of the name of the
  atom you wish to increase"
  [v-atom-kwd]
  (let [v-atom (as-var v-atom-kwd)
        old-val @v-atom]
    (swap! v-atom (partial * 6/5))))

(defn- decrease
  "For documentation purposes, pass a keyword version of the name of the
  atom you wish to decrease"
  [v-atom-kwd]
  (let [v-atom (as-var v-atom-kwd)
        old-val @v-atom]
    (swap! v-atom (partial * 5/6))))

(defn handle-keypress []
  (let [cur-key (q/key-as-keyword)]
    (condp = cur-key
      :a     (increase :da)
      :A     (decrease :da)
      :b     (increase :db)
      :B     (decrease :db)
      :f     (increase :fr)
      :F     (decrease :fr)
      :k     (increase :kr)
      :K     (decrease :kr)
      :p     (core/reset-params 0.5) ; move params halfway back to default
      :P     (core/reset-params 1.0) ; reset params to default values
      :r     (increase :rr)
      :R     (decrease :rr)
      :s     (swap! show-param-values not)
      :t     (increase :t)
      :T     (decrease :t)
      :v     (swap! show-values not)
      :!     (setup)
      :c     (color/inc-color)
      :C     (color/dec-color)
      ;; TODO: help on right side
      ;; :h     (clojure.repl/source 'handle-keypress)
      ;; TODO: key to reset parameter values
      nil)))

(defn run []
  (q/defsketch example
    :size [w h]
    :title "foobar"
    :settings #(q/smooth 2)
    :renderer :opengl
    :key-pressed handle-keypress
    :setup setup
    :draw draw))
