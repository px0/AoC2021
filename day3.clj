(ns day3
  (:require [clojure.string :as str]))

  (def +puzzle-test+ (str/split-lines "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"))

(def +puzzle+ +puzzle-test+)
(def +puzzle+ (-> "day3.input" slurp str/split-lines))

;;; part1
 (def bits (for [idx (range (count (first +puzzle+)))
                :let [bit-row (map (fn [^String num]
                                     (.charAt num idx)) +puzzle+)
                      freqs (frequencies bit-row)
                      zero-bits (get freqs \0)
                      one-bits (get freqs \1)]]
            (if (> one-bits zero-bits)
              {:gamma 1 :epsilon 0}
              {:gamma 0 :epsilon 1})))
  
(def gamma-bitstr (apply str (map :gamma bits)))
(def epsilon-bitstr (apply str (map :epsilon bits)))

(*
 (Integer/parseInt gamma-bitstr 2)
 (Integer/parseInt epsilon-bitstr 2));; => 4006064


;;; part 2

(defn bit-frequency-at-idx [numbers idx]
  (->> numbers
       (map (fn [^String num]
              (.charAt num idx)))
       (frequencies)))

(defn rating-value [input rating-fn]
  (loop [numbers input
         idx 0]
    (if (= 1 (count numbers))
      (first numbers)
      (let [next-bit-freqs (bit-frequency-at-idx numbers idx)
            next-bit (rating-fn next-bit-freqs)
            matching-nums (filter (fn [^String num]
                                    (= next-bit (.charAt num idx))) numbers)]
        (recur matching-nums (inc idx))))))

(def oxygen-rating (rating-value
                    +puzzle+
                    (fn [bit-freqs]
                      (if (>= (get bit-freqs \1 0)
                              (get bit-freqs \0 0))
                        \1
                        \0))))

(def co2-rating (rating-value
                 +puzzle+
                 (fn [bit-freqs]
                   (if (>= (get bit-freqs \1 0)
                           (get bit-freqs \0 0))
                     \0
                     \1))))

(*
 (Integer/parseInt oxygen-rating 2)
 (Integer/parseInt co2-rating 2));; => 5941884
