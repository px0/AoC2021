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
   (print-grid grid #{}))

  ([grid hl]
   (let [wh (Math/sqrt (count (keys grid)))]
     (print-grid grid hl wh wh)))

  ([grid hl w h]
   (doseq [y (range h)]
         (doseq [x (range w)]
           (print
            (if (contains? (set hl) [x y])
              (utils/with-ansi :bright
                (format "%x" (get grid [x y] 0)))
              (format "%x" (get grid [x y] 0)))))
         (print "\n"))))


(defn get-path [dist goal]
  (loop [cur (get dist goal)
         acc (list goal)]
    (if-let [next (get dist (:parent cur))]
      (recur next (conj acc (:parent cur)))
      acc)))


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

    
(defn djikstra [graph start neighbours cost]
  (let [dist (atom (-> (->> (keys graph)
                            (map (fn [pos]
                                   [pos {:cost Integer/MAX_VALUE
                                         :parent nil}]))
                            (into {}))
                       (assoc start {:cost 0 :parent nil})))
        pq (PriorityQueue. #(compare ^int (:cost (second %1))
                                     ^int (:cost (second %2))))]

    (doseq [[pos {cost :cost}] (seq @dist)]
      (.add pq [pos cost]))

    (loop [[cur curcost] [start 0]]

      (doseq [n (neighbours cur)
              :let [ntotal (+ curcost (cost n))
                    {cost :cost} (@dist n)]]
        (when (< ntotal cost)
            (swap! dist assoc n {:cost ntotal :parent cur})
            (.remove pq [n cost])
            (.add pq [n ntotal])))

      (if-let [next-cur-cost (.poll pq)]
        (recur next-cur-cost)
        @dist))))




(time
 (let [+p+ (slurp "day15.input")
       ;;gridmap (repeat-grid (grid->map +p+) 5)
       gridmap (grid->map +p+)
       
       [dim-x dim-y] (grid-dimension gridmap)
       endnode [(dec dim-x) (dec dim-y)]
      _ (do
          (def gridmap gridmap))
      dist (djikstra
            gridmap
            [0 0]
            (partial neighbours gridmap)
            (partial cost gridmap)
            )]
   (print-grid gridmap (get-path dist endnode))

  endnode
))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn cost-a* [gridmap [end-x end-y] [x y :as pos]]
  (+ (Math/abs (- end-x x))
     (Math/abs (- end-y y))
     (gridmap pos)))

    
(defn a* [graph start neighbours goal cost pathcost]
  (let [dist (atom (-> (->> (keys graph)
                            (map (fn [pos]
                                   [pos {:cost Integer/MAX_VALUE
                                         :pathcost 0
                                         :parent nil}]))
                            (into {}))
                       (assoc start {:cost 0 :parent nil :pathcost 0})))
        pq (PriorityQueue. #(compare ^int (second %1)
                                     ^int (second %2)))]

    (loop [[cur curcost] [start 0]
           visited (set [start])]
      ;;(prn 'looking-at cur (@dist cur))

      (doseq [n (neighbours cur)
              :when (not (contains? visited n))
              :let [ncost (+ curcost (cost n))
                    {cost :cost} (@dist n)]]
        (when (< ncost cost)
          (swap! dist assoc n {:cost ncost
                               :pathcost (+ (:pathcost (@dist cur))
                                            (pathcost n))
                               :parent cur})
          (when (.contains pq [n cost])
            (.remove pq [n cost]))
          (.add pq [n ncost])))

      (let [[next-cur _ :as next] (.poll pq)]
        (if (= next-cur goal)
          (do (def dist @dist)
              (@dist goal))
          (recur next (conj visited cur)))))))

  
;;(def +p+ (slurp "day15.input"))

(time
 (let [+p+ (slurp "day15.input")
       gridmap (grid->map +p+)
       gridmap (repeat-grid (grid->map +p+) 5)
       [dim-x dim-y] (grid-dimension gridmap)
       endnode [(dec dim-x) (dec dim-y)]
      _ (do
          (def gridmap gridmap))
      goalnode (a*
            gridmap
            [0 0]
            (partial neighbours gridmap)
            endnode
            (partial cost gridmap #_endnode) ;; i get the wrong result with the heuristic?
            (partial cost gridmap)
            )]
   (def endnode endnode)
   (prn goalnode)
   (spit "/tmp/path.txt"
         (with-out-str
           (print-grid gridmap (get-path dist endnode))))
  goalnode
));; => {:cost 2925, :pathcost 2925, :parent [499 498]}



