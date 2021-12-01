(ns day1)
(require '[clojure.string :as str])

(def +puzzle+ (->> "day1.input" slurp str/split-lines (map #(Integer/parseInt %))))

(defn sliding-window [n seq]
  (if (-> seq count (> n))
    (lazy-seq 
     (cons (take n seq) (sliding-window n (rest seq))))
    (list seq)))

;; part 1
(->> (sliding-window 2 +puzzle+)
     (map (partial apply <))
     (frequencies)) ;; => {true 1400, false 599}

;; part 2
(->> (sliding-window 3 +puzzle+)
     (map (partial apply +))
     (sliding-window 2)
     (map (partial apply <))
     (frequencies)) ;; => {true 1429, false 568}
