(ns day5
  (:require [clojure.string :as str]))

(def +puzzle+
  (str/split-lines "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"))

(def +puzzle+ (-> "day5.input" slurp str/split-lines))

(defn parse-input [lines]
  (for [line lines
        :let [coords (str/split line #"(,| -> )")
              [x1 y1 x2 y2] (map #(Integer/parseInt %) coords)]]
    {:x1 x1 :y1 y1 :x2 x2 :y2 y2}))

(defn create-grid-part1 [parsed-input]
  (let [grid (atom {})]
    (doseq [{:keys [x1 x2 y1 y2]} parsed-input
            :when (or (= x1 x2) (= y1 y2))
            x (range (min x1 x2) (inc (max x1 x2)))
            y (range (min y1 y2) (inc (max y1 y2)))]
      (swap! grid update [x y] (fnil inc 0)))
    @grid))

(defn count-intersections [grid]
  (reduce-kv (fn [crossings _ lines]
               (if (>= lines 2)
                 (inc crossings)
                 crossings))
             0
             grid))

;;; part1
(-> +puzzle+
    (parse-input)
    (create-grid-part1)
    (count-intersections)) ;; => 4745

;;; part2
(defn coord-seq [x1 y1 x2 y2]
  (lazy-seq
   (cons [x1 y1]
         (when-not (and (= x1 x2) (= y1 y2))
           (coord-seq (cond (> x1 x2) (dec x1)
                            (< x1 x2) (inc x1)
                            :else x1)
                      (cond (> y1 y2) (dec y1)
                            (< y1 y2) (inc y1)
                            :else y1)
                      x2 y2)))))

(defn create-grid-part2 [parsed-input]
  (let [grid (atom {})]
    (doseq [{:keys [x1 x2 y1 y2]} parsed-input
            x-y (coord-seq x1 y1 x2 y2)]
      (swap! grid update x-y (fnil inc 0)))
    @grid))

(-> +puzzle+
    (parse-input)
    (create-grid-part2)
    (count-intersections));; => 18442
