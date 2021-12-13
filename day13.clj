(ns day13
  (:require [utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(def +pp+ "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0")

(def +pf+ "fold along y=7
fold along x=5")

(defn make-grid [s]
  (reduce (fn [acc l]
            (assoc acc (utils/split-commas-to-ints l) true))
            {}
            (str/split-lines s)))
          

(defn grid-width [g]
  (apply max (map first (keys g))))

(defn grid-height [g]
  (apply max (map second (keys g))))

(defn print-grid [g]
  (doseq [y (range (inc (grid-height g)))]
    (println
     (apply str
            (for [x (range (inc (grid-width g)))]
              (if (contains? g (list x y)) "*" " "))))))
          

;; (defn split-grid [g x-or-y val]
;;   (->>
;;    (group-by (fn [[pos _]] (> (x-or-y pos) val)) g)
;;    (vals)
;;   (map #(into {} %))))

(defn fold-grid
  "fold grid G along X-OR-Y-IDX which is 0 for x and 1 for y, and the
  value VAL"
  [g x-or-y-idx val]
  (let [max-dim (apply max (map #(nth % x-or-y-idx) (keys g)))
        other {0 1 1 0}]
    (reduce-kv
     (fn [g k v]
       (let [x-or-y (nth k x-or-y-idx)
             x-or-y' (if (>= x-or-y val)
                       (- max-dim x-or-y)
                       x-or-y)
             other-x-or-y-idx (get other x-or-y-idx)
             other-x-or-y (nth k other-x-or-y-idx)
             new-coords (-> ['x 'y]
                            (assoc x-or-y-idx x-or-y')
                            (assoc other-x-or-y-idx other-x-or-y)
                            )]
         (assoc g new-coords v)))
     {} g)))


(->  (make-grid +pp+)
     (fold-grid 1 7)
     (fold-grid 0 5)
     (print-grid))

;;; part 1
(def +puzzle-grid+ (slurp "day13.input"))
(-> 
 (make-grid +puzzle-grid+)
 (fold-grid 0 655)
 (count));; => 781


;;; part 2
(->  (make-grid +puzzle-grid+)
     (fold-grid 0 655)
     (fold-grid 1 447)
     (fold-grid 0 327)
     (fold-grid 1 223)
     (fold-grid 0 163)
     (fold-grid 1 111)
     (fold-grid 0 81)
     (fold-grid 1 55)
     (fold-grid 0 40)
     (fold-grid 1 27)
     (fold-grid 1 13)
     (fold-grid 1 6)
     (print-grid))

