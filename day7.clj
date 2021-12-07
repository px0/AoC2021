(ns day7
  (:require utils))

(def +puzzle+ [16,1,2,0,4,2,7,1,2,14])
(def +puzzle+ (-> "day7.input" slurp utils/split-commas-to-ints))

(defn fuel-cost [to-pos cur-pos]
  (Math/abs (- to-pos cur-pos)))

(defn total-fuel-cost [single-cost-fn input pos]
  (reduce + (map (partial single-cost-fn pos) input)))

(defn binary-search [input cost-fn]
  (loop [min-pos (apply min input)
         min-cost (cost-fn input min-pos)
         max-pos (apply max input)
         max-cost (cost-fn input max-pos)]


         (let [mid-pos (int (/ (+ max-pos min-pos) 2))
               mid-cost (cost-fn input mid-pos)]
           
           (println "min" min-pos "(" min-cost ")"
                    "max" max-pos "(" max-cost ")"
                    "=> middle" mid-pos "(" mid-cost ")")
           
           (cond
             (or (= mid-pos min-pos) (= mid-pos max-pos))
             (min min-cost mid-cost max-cost)

             (< min-cost mid-cost max-cost)
             (recur min-pos min-cost mid-pos mid-cost)

             (< mid-cost min-cost max-cost)
             (recur mid-pos mid-cost min-pos min-cost)

             :else "i fucked up"))))

;;; part 1
(binary-search +puzzle+ (partial total-fuel-cost fuel-cost));; => 336721

;;; part 2

(defn fuel-cost-part2 [to-pos cur-pos]
  (let [steps (Math/abs (- to-pos cur-pos))]
    (reduce +(range 1 (inc steps)))))

(binary-search +puzzle+ (partial total-fuel-cost fuel-cost-part2));; => 91638945
