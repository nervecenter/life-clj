(ns life-clj.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def cell-size 10)
(def size-x 500)
(def size-y 500)
(def cells-x (/ size-x cell-size))
(def cells-y (/ size-y cell-size))
(def coords (for [x (range 0 cells-x)
                  y (range 0 cells-y)]
              [x y]))

(defn random-cell []
  (if (> 30 (rand-int 100))
    :alive
    :dead))

(defn random-grid []
  (let [cells (for [x (range 0 cells-x)
                    y (range 0 cells-y)]
                (random-cell))]
    (zipmap coords cells)))

(defn setup []
  (q/frame-rate 2)
  (random-grid))

(defn alive? [cell-val]
  (if (= cell-val :alive)
    true
    false))

(defn neighbors-alive [grid x y]
  (count
   (filter alive?
     [(grid [(- x 1) (- y 1)]) (grid [x (- y 1)]) (grid [(+ x 1) (- y 1)])
      (grid [(- x 1) y])                          (grid [(+ x 1) y])
      (grid [(- x 1) (+ y 1)]) (grid [x (+ y 1)]) (grid [(+ x 1) (+ y 1)])])))

(defn decide-fate [grid x y]
  (let [live-neighbors (neighbors-alive grid x y)]
    (if (alive? (grid [x y]))
      (cond
        (< live-neighbors 2) :dead
        (or (= live-neighbors 2) (= live-neighbors 3)) :alive
        (> live-neighbors 3) :dead)
      (if (= live-neighbors 3)
        :alive
        :dead))))

(defn update-grid [grid]
  (let [new-cells (for [x (range 0 cells-x)
                        y (range 0 cells-y)]
                    (decide-fate grid x y))]
    (zipmap coords new-cells)))

(defn draw-grid [grid]
  ; White background
  (q/background 255)
  ; Black rectangles
  (q/fill-float 0)
  (doseq [coords (keys grid)]
    (if (alive? (grid coords))
      (q/rect
       (* 10 (get coords 0))
       (* 10 (get coords 1))
       cell-size
       cell-size)
      nil)))

(q/defsketch life-clj
  :title "Conway's Game of Life - Clojure"
  :size [size-x size-y]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-grid
  :draw draw-grid
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
