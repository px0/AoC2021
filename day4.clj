(ns day4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def +puzzle+-demo
  (->
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"
  (str/split-lines)))

(def +puzzle+ +puzzle+-demo)

(def +puzzle+ (-> "day4.input" slurp str/split-lines))

(def row-num 5)


(defn make-board-cols
  "Parse raw strings into board columns"
  [{raw-cols :raw :as board}]
  (let [cols (for [col raw-cols]
                           (->> (str/split col #"\s+")
                                (remove empty?)
                                (map #(Integer/parseInt %))))]
    (assoc board
           :cols cols
           :cols-set (map set cols))))

(defn make-board-rows
  "Figure out the board rows by transposing the columns"
  [{cols :cols :as board}]
  (let [rows (apply (partial map list) cols)]
    (assoc board
           :rows rows
           :rows-set (map set rows))))

(defn make-board
  "Create board datastructure from column strings"
  [col-strs]
  (-> {:raw col-strs}
      (make-board-cols)
      (make-board-rows)
      ((fn [board] 
         (assoc board
                :rows-cols-set (set/union (:rows-set board) (:cols-set board))
                :all-numbers (set (flatten (:cols board))))))))

(defn parse-boards
  "Create boards from input"
  [input]
  (cons (make-board (take row-num input))
        (when-let [remainder (seq (drop (inc row-num) input))]
          (parse-boards remainder))))

(defn parse-input
  "Parse numbers and boards from the input. Use this one directly."
  [input]
  {:numbers (map #(Integer/parseInt %) (str/split (first input) #","))
   :boards (parse-boards (drop 2 input))})


(defn bingo?
  "Checks the matching row/col if bingo, else nil"
  [numbers {:keys [rows-cols-set all-numbers]}]
  (when-let [matches (set/intersection (set numbers) all-numbers)]
    (when (>= (count matches) 5)
      (letfn [(match-reduce-fn [_ col-or-row]
                (when (set/subset? col-or-row (set numbers))
                  (reduced col-or-row)))]
        (reduce match-reduce-fn rows-cols-set)))))

(defn calculate-score-part1 [{:keys [winning-board winning-streak winning-number]}]
  (let [{:keys [all-numbers]} winning-board
        unmarked-numbers (set/difference (set all-numbers) (set winning-streak))]
    (prn winning-number unmarked-numbers (apply + unmarked-numbers))
    (* winning-number
       (apply + unmarked-numbers))))

;;; part 1 - find the winning board
(->>
 (let [{:keys [numbers boards]} (parse-input +puzzle+)]
   (def numbers numbers)
   (def boards boards)
   (for [idx (range (count numbers))
         :let [current-numbers (set (take idx numbers))]
         board boards
         :when (bingo? current-numbers board)]
     {:winning-board board
      :winning-idx idx
      :winning-number (nth numbers (dec idx))
      :winning-streak (take idx numbers)
      :winning-row-col (bingo? current-numbers board)}))
 (first)
 ;; (def winners)
 (calculate-score-part1)
 );; => 42351
