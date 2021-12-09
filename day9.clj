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

(defn risk-rating [val]
  (inc val))


;;; part 1
(reduce +
         (for [y (range (count +puzzle+))
                      x (range (count (first +puzzle+)))
                      :when (low-point? +puzzle+ [x y])]
                  (inc (val-at +puzzle+ [x y]))));; => 436

;;; part 2
(defn find-basin-at-pos
  ([grid pos]
   (find-basin-at-pos #{} grid pos))

  ([visited grid [x y]]
   (when-not (or (contains? #{nil 9} (val-at grid [x y]))
                 (contains? visited [x y]))
     (set (cons [x y]
                (mapcat (partial find-basin-at-pos (conj visited [x y]) grid)
                        [[(inc x) y]
                         [(dec x) y]
                         [x       (inc y)]
                         [x       (dec y)]]))))))


;; elegant & works but slow
(->>  (for [y (range (count +puzzle+))
            x (range (count (first +puzzle+)))]
        (find-basin-at-pos +puzzle+ [x y]))
      (remove nil?)
      (set)
      (sort-by count >)
      (take 3)
      (map count)
      (reduce *))


(let [visited (atom #{})]
(->>  (for [y (range (count +puzzle+))
            x (range (count (first +puzzle+)))
            :when (not (contains? @visited [x y]))
            :let [basin (find-basin-at-pos +puzzle+ [x y])]]
        (do
          (swap! visited (partial set/union basin))
          (prn 'visited-count (count @visited))
        basin)
        )
      (remove nil?)
      (set)
      (sort-by count >)
      (take 3)
      (map count)
      (reduce *)))

;; unfinished

(loop [x 0
       y 0
       visited #{}
       basins #{}]
  (if (= [x y] [(count (first +puzzle+) (count +puzzle+)])
         basins
         
  (if-not (contains? visited [x y])
    
    (recur (if (= x (count (first +puzzle+))) 0 (inc x))
           (if (= x (count (first +puzzle+))) (inc y) y)
           )))
  (let [new-basin (find-basin-at-pos +puzzle+ [x y])]
    )
  )
(->>  (for [y (range (count +puzzle+))
            x (range (count (first +puzzle+)))]
        (find-basin-at-pos +puzzle+ [x y]))
      (remove nil?)
      (set)
      (sort-by count >)
      (take 3)
      (map count)
      (reduce *))
