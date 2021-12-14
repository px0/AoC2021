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
          

(defn print-grid [g]
  (let [width (apply max (map first (keys g)))
        height (apply max (map second (keys g)))]
  (doseq [y (range (inc height))]
    (println
     (apply str
            (for [x (range (inc width))]
              (if (contains? g (list x y)) "#" " ")))))))
  

(defn fold-grid-y
  "fold grid G along FOLD-LINE"
  [g fold-line]
  (let [max-y (+ fold-line fold-line)
        grid (reduce-kv (fn [g [x y] v]
                          (let [y' (if (> y fold-line)
                                          (- max-y y)
                                          y)]
                           
                            (assoc g (list x y') v)))
                        {} g)]
    grid))

  (defn fold-grid-x
  "fold grid G along FOLD-LINE"
  [g fold-line]
  (let [max-x (+ fold-line fold-line)
        grid (reduce-kv (fn [g [x y] v]
                          (let [x' (if (> x fold-line)
                                          (- max-x x)
                                          x)]
                            (assoc g (list x' y) v)))
                        {} g)]
    grid))

  
(->  (make-grid +pp+)
     (fold-grid-y 7)
     (fold-grid-x 5)
     (print-grid))

;;; part 1
(def +puzzle-grid+ (slurp "day13.input"))
(-> 
 (make-grid +puzzle-grid+)
 (fold-grid-x 655)
 (count));; => 781


;;; part 2
(with-out-str
(->  (make-grid +puzzle-grid+)
     (fold-grid-x 655)
     (fold-grid-y 447)
     (fold-grid-x 327)
     (fold-grid-y 223)
     (fold-grid-x 163)
     (fold-grid-y 111)
     (fold-grid-x 81)
     (fold-grid-y 55)
     (fold-grid-x 40)
     (fold-grid-y 27)
     (fold-grid-y 13)
     (fold-grid-y 6)
     (print-grid))) ;; =>
;;"###  #### ###   ##   ##    ## ###  ### 
;; #  # #    #  # #  # #  #    # #  # #  #
;; #  # ###  #  # #    #       # #  # ### 
;; ###  #    ###  #    # ##    # ###  #  #
;; #    #    # #  #  # #  # #  # #    #  #
;; #    #### #  #  ##   ###  ##  #    ### 
"
