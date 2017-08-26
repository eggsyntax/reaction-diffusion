(ns reactiondiffusion.color)

;; Color schemes
(defn c01 [a b]
  [(* a 255) 40 (* b 255)])

;; smooth a:green, b:blue
(defn c02 [a b]
  [80 (* a 255) (* b 255)])

;; show greater
(defn c03 [a b]
  (cond (> a (+ b 0.05)) [255 255 255]
        (> b (+ a 0.05)) [  0   0   0]
        :else            [127   0   0]))

;; show greater with fades
(defn c04 [a b]
  (cond (> a (+ b 0.01)) [ 30 (* a 255)       0]
        (> b (+ a 0.01)) [ 30        0 (* b 255)]
        :else            [180       90       90]))

(defn c05 [a b]
  ;; [(* a 255) (* b 255) 40]
  (if (> a b)
    [220 240 230]
    [10 15 30])
  )

(defn c06 [a b]
  (let [diff (- a b)] ; -1..1
    (condp < diff
      0.0  [220 240 230]
      -0.5 [120 140 130]
      ,    [ 10  15  30])))

(def colors [c01, c02, c03, c04, c05, c06])

(def cur-color (atom 0))

(defn change-color-index
  ([f]
   (let [cnt (count colors)]
     (swap! cur-color
            (fn [c-i] (mod (f c-i) cnt))))))

(defn inc-color [] (change-color-index inc))

(defn dec-color [] (change-color-index dec))
