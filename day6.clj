(ns day6
  (:require [clojure.string :as str]))

(def +puzzle+ [3,4,3,1,2])
(def +puzzle+ (-> "day6.input"
                  slurp
                  (str/replace #"\n" "")
                  (str/split #",")
                  (->> (map #(Integer/parseInt %)))))

(defn next-day [input]
  (->> input
       (map (fn [age] (let [next-age (dec age)]
                        (if (neg? next-age)
                          [6 8]
                          next-age))))
       (flatten)))


;;; part 1
(-> (iterate next-day +puzzle+)
    (nth 80)
    (count));; => 373378


;;; part 2

(defn less-naive-next-day [input]
  {8 (get input 0 0)
   7 (get input 8 0)
   6 (+ (get input 7 0) (get input 0 0))
   5 (get input 6 0)
   4 (get input 5 0)
   3 (get input 4 0)
   2 (get input 3 0)
   1 (get input 2 0)
   0 (get input 1 0)})

(defn count-fish [fish-freqs]
  (reduce + (vals fish-freqs)))

(-> (iterate less-naive-next-day (frequencies +puzzle+))
    (nth 256)
    (count-fish));; => 1682576647495
