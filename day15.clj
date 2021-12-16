(ns day15
  (:require [utils]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.set :as set]))
(import java.util.PriorityQueue)


(def +p+ "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(defn grid-dimension [grid]
  [(inc (apply max (map first (keys grid))))
   (inc (apply max (map second (keys grid))))])
  
(defn grid->map [s]
  (into (hash-map)
        (let [lines (str/split-lines s)]
          (for [x (range (count (first lines)))
                y (range (count lines))]
            [[x y] (-> (nth lines y)
                       (#(.charAt % x))
                       (str)
                       (utils/parse-int))]))))

(defn repeat-grid [grid factor]
  (let [gg (atom grid)
        dim-x (inc (apply max (map first (keys grid))))
        dim-y (inc (apply max (map second (keys grid))))]
    (doseq [idx-x (range factor)
            idx-y (range factor)
            [[x y] val] grid]
      (swap! gg assoc [(+ (* dim-x idx-x) x)
                       (+ (* dim-y idx-y) y)]
             (let [v (+ val idx-x idx-y)]
               (if (> v 9) (inc (mod v 10)) (mod v 10)))))
     @gg))
  
        
      

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
                                     (format "%x" (get grid [x y] 0)))))))
    "\n")))





(defn neighbours [gridmap [x y]]
  (for [c [[(inc x) y]
           [x (inc y)]
           [(dec x) y]
           [x (dec y)]
           ]
        :when (contains? gridmap c)]
    c))

(defn cost [gridmap pos]
  (gridmap pos))

    
(defn djikstra [graph start goal neighbours cost]
  (let [dist (atom (-> (->> (keys graph)
                            (map (fn [pos]
                                   [pos {:cost Integer/MAX_VALUE
                                         :parent nil}]))
                            (into {}))
                       (assoc start {:cost 0 :parent nil})))
        pq (PriorityQueue. #(compare (:cost (second %1))
                                     (:cost (second %2))))]

    ;; (def dist dist) (def pq pq)
    
    (doseq [[pos {cost :cost}] (seq @dist)]
      (.add pq [pos cost]))

    (loop [[cur curcost] [start 0]]
          
          (doseq [n (neighbours cur)
                  :let [ntotal (+ curcost (cost n))]]
            (let [{:keys [cost]} (@dist n)]
              (when (< ntotal cost)
                (swap! dist assoc n {:cost ntotal :parent cur})
                (.remove pq [n cost])
                (.add pq [n ntotal]))))
          
          (if-let [next-cur-cost (.poll pq)]
            (recur next-cur-cost)
              @dist))))

;;(def +p+ (slurp "day15.input"))

(time
 (let [+p+ (slurp "day15.input")
       gridmap (repeat-grid (grid->map +p+) 5)
      [dim-x dim-y] (grid-dimension gridmap)
      _ (do
          (def gridmap gridmap))
      dist (djikstra
            gridmap
            [0 0]
            [(dec dim-x) (dec dim-y)]
            (partial neighbours gridmap)
            (partial cost gridmap))]
  (def dist dist)
  (dist [(dec dim-x) (dec dim-y)]))
)



