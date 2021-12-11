(ns day11
  (:require [utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn grid->map [s]
  (into (hash-map)
        (let [lines (str/split-lines s)]
          (for [x (range (count (first lines)))
                y (range (count lines))]
            [[x y] (-> (nth lines y)
                       (#(.charAt % x))
                       (str)
                       (utils/parse-int))]))))
        
(def +puzzle+ (-> "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"
                  grid->map))

(defn print-grid
  ([grid]
   (let [wh (Math/sqrt (count (keys grid)))]
     (print-grid grid wh wh)))
   
  ([grid w h]
   (println
    (apply str
           (interpose "\n"
                      (for [y (range h)]
                        (apply str (for [x (range w)]
                                     (format "%x" (grid [x y])))))))
    "\n")))

(defn neighbours [grid [x y]]
  (let [neighbours [[-1  1] [0  1] [1  1]
                    [-1  0]        [1  0]
                    [-1 -1] [0 -1] [1 -1]]
        neighbours-pos (map (fn [[x' y']]
                              [(+ x x') (+ y y')])
                            neighbours)]
    (filter (partial contains? grid) neighbours-pos)))

(defn swap-grid [grid pos f & vals]
  (assoc grid pos (apply f (get grid pos) vals)))

(defn step-inc [grid]
  (reduce-kv (fn [grid k v]
               (assoc grid k (inc v))) {} grid))

(defn -inc-neighbours [grid pos]
  (reduce (fn [grid n-pos]
            (swap-grid grid n-pos inc))
          grid (neighbours grid pos)))

(defn step-zero-flashers [grid flashers]
  (reduce (fn [grid pos]
            (assoc grid pos 0)) grid flashers))

(defn step-flashes [grid *flashes]
  (loop [grid grid]

    (let [flashers (for [[pos v] grid
                         :when (>= v 10)
                         :when (not (contains? @*flashes pos))]
                     pos)
          new-grid (reduce (fn [grid f-pos]
                             (swap! *flashes conj f-pos)
                             (-inc-neighbours grid f-pos))
                           grid flashers)]
      ;; (println "----stepflashes")
      ;; (print-grid new-grid)

      (if (= new-grid grid)
        (step-zero-flashers grid @*flashes)
        (recur new-grid)))))
  

(defn step [grid]
  (let [*flashes (atom #{})]
    (-> grid
         (step-inc)
         (step-flashes *flashes))
    (count @*flashes)))

(defn count-flashes [grid n]
  (loop [counter 1
         grid grid
         flashes 0]
    (println "=== Step" counter)
           
    (let [*flashes (atom #{})
          new-grid (-> grid
                       (step-inc)
                       (step-flashes *flashes))]
      (print-grid new-grid)
      (if (<= counter n)
        (recur (inc counter)
               new-grid
               (+ flashes (count @*flashes)))
        flashes))))


;;; part 1
(count-flashes (grid->map "1553421288
5255384882
1224315732
4258242274
1658564216
6872651182
5775552238
5622545172
8766672318
2178374835") 100);; => 1679

(defn simultaneous-flashes [grid]
  (loop [counter 1
         grid grid]
    (println "=== Step" counter)
           
    (let [*flashes (atom #{})
          new-grid (-> grid
                       (step-inc)
                       (step-flashes *flashes))]
      (print-grid new-grid)
      (if (not= (count @*flashes) 100)
        (recur (inc counter)
               new-grid)
        counter))))

(simultaneous-flashes (grid->map "1553421288
5255384882
1224315732
4258242274
1658564216
6872651182
5775552238
5622545172
8766672318
2178374835"));; => 519
