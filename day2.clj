(ns day2)

(require '[clojure.string :as str])

(def +puzzle+ (->> "day2.input"
                   slurp
                   str/split-lines
                   (map (fn [line]
                          (let [[_ instruction value]
                                (re-matches #"(\w+) (\d+)" line)]
                            [instruction (Integer/parseInt value)])))))

(defn forward [value state]
  (update state :xpos (partial + value)))

(defn down [value state]
  (update state :depth (partial + value)))

(defn up [value state]
  (update state :depth #(- % value)))

(defn interpret [instructions]
  (reduce (fn [state [instruction value]]
            (let [func (-> instruction symbol resolve)]
              (func value state)))
          {:depth 0 :xpos 0}
          instructions))

;;; part 1
(apply * (vals (interpret +puzzle+)));; => 2027977

;;; part 2

(defn forward [value state]
  (-> state
      (update :xpos (partial + value))
      (update :depth (partial + (* (:aim state) value)))))

(defn down [value state]
  (update state :aim (partial + value)))

(defn up [value state]
  (update state :aim #(- % value)))

(defn interpret [instructions]
  (reduce (fn [state [instruction value]]
            (let [func (-> instruction symbol resolve)]
              (func value state)))
          {:depth 0 :xpos 0 :aim 0}
          instructions))

(apply * ((juxt :depth :xpos) (interpret +puzzle+)));; => 1903644897 
