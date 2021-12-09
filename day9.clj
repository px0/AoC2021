(ns day9
  (:require utils
            [clojure.string :as str]
            [clojure.set :as set]))

(def +puzzle+ (str/split-lines "2199943210
3987894921
9856789892
8767896789
9899965678"))

(def +puzzle+ (-> "day9.input" slurp str/split-lines))


(defn val-at [grid [x y]]
  (try
    (-> grid
        (nth y)
        (.charAt x)
        (str)
        (utils/parse-int))
    (catch Throwable _)))

(defn neighbours [grid [x y]]
  (let [val-at* (partial val-at grid)]
    (->> [(val-at* [(inc x)  y])
          (val-at* [(dec x)  y])
          (val-at* [x        (inc y)])
          (val-at* [x        (dec y)])]
         (remove nil?))))

(defn low-point? [grid pos]
  (when (every? (partial < (val-at grid pos))
          (neighbours grid pos))
    pos))

(defn risk-rating [grid pos]
  (inc (val-at grid pos)))


;;; part 1
(reduce +
         (for [y (range (count +puzzle+))
                      x (range (count (first +puzzle+)))
                      :when (low-point? +puzzle+ [x y])]
                  (risk-rating +puzzle+ [x y])));; => 436

;;; part 2
(defn find-basin-at-pos
  ([grid pos]
   (let [visited (atom #{})]
     (find-basin-at-pos visited grid pos)))

  ([visited grid [x y]]
   (when-not (or (contains? #{nil 9} (val-at grid [x y]))
                 (contains? @visited [x y]))
     (swap! visited conj [x y])
     (set (cons [x y]
                (mapcat (partial find-basin-at-pos visited grid)
                        [[(inc x) y]
                         [(dec x) y]
                         [x       (inc y)]
                         [x       (dec y)]]))))))


(let [visited (atom #{})]
  (->>  (for [y (range (count +puzzle+))
              x (range (count (first +puzzle+)))
              :when (not (contains? @visited [x y]))
              :let [basin (find-basin-at-pos visited +puzzle+ [x y])]]
          basin)
        (remove nil?)
        (set)
        (sort-by count >)
        (take 3)
        (map count)
        (reduce *)));; => 1317792
