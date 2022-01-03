(ns day17
  (:require
   [clojure.edn :as edn]
   [clojure.test :as test]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.walk :as walk]
   [clojure.set :as set]))

(defn shoot
  "shoot the probe, return a lazy seq of coordinates"
  ([x-vel y-vel]
   (shoot 0 0 x-vel y-vel))
  ([pos-x pos-y x-vel y-vel]
  (lazy-seq
   (cons [(+ pos-x x-vel)
          (+ pos-y y-vel)]
         (shoot
          (+ pos-x x-vel)
          (+ pos-y y-vel)
          (cond (neg? x-vel) (inc x-vel)
                (pos? x-vel) (dec x-vel)
                (zero? x-vel) 0)
          (dec y-vel))))))

(defn bullseye? [[target-x-min target-x-max target-y-min target-y-max] [x y]]
  (and (<= target-x-min x target-x-max)
       (<= target-y-min y target-y-max)))

(def demo-target [20 30 -10 -5])

(defn past-target?
  [[target-x-min target-x-max target-y-min target-y-max] [x y]]
  (or (> x target-x-max)
      (< y target-y-min)))

(defn probe-flight-coords [target trajectory]
  (take-while (partial (complement past-target?) target) (apply shoot trajectory)))

(defn hits-target? [target trajectory]
  (boolean
   (some (partial bullseye? target)
        (probe-flight-coords target trajectory))))

(test/deftest tests
  (test/is (true? (hits-target? demo-target [7 2])))
  (test/is (false? (hits-target? demo-target [71 20])))
  (test/is (true? (hits-target? demo-target [6 3])))
  (test/is (true? (hits-target? demo-target [9 0])))
  (test/is (false? (hits-target? demo-target [17 4])))
  )

;;; part 1
(def puzzle-target [138 184, -125 -71])

(apply max
 (for [x (range 0 185) ; more than that and the first step goes past the target
       y (range -500 500) ; ¯\_(ツ)_/¯
       :when (hits-target? puzzle-target [x y])
       :let [coords (probe-flight-coords puzzle-target [x y])
             ys (map second coords)
             max-y (apply max ys)]]
   max-y));; => 7750

;;; part 2
(count
 (for [x (range 0 185) ; more than that and the first step goes past the target
       y (range -500 500) ; ¯\_(ツ)_/¯
       :when (hits-target? puzzle-target [x y])]
   [x y]));; => 4120

