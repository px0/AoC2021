(ns day10
  (:require [utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(def +puzzle+ (->> "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"
                  str/split-lines
                  ))

(def +puzzle+ (->> "day10.input" slurp str/split-lines ))

(def mapping {\( \)
              \{ \}
              \[ \]
              \< \>})

(def points {\) 3
             \] 57
             \} 1197
             \> 25137})


(defn parse [line]
  (let [*opensym-stack (atom '())
        openings (set (keys mapping))
        closings (set (vals mapping))]
    (doseq [sym line]
      (cond
        (contains? openings sym)
        (swap! *opensym-stack conj sym)

        (contains? closings sym)
        (if (= sym (mapping (peek @*opensym-stack)))
          (swap! *opensym-stack pop)
          (throw (ex-info (str "Expected " (mapping (peek @*opensym-stack))
                               " but got " sym " instead")
                          {:type :error
                           :symbol sym})))))
    ;; return missing closers
    @*opensym-stack))


;;; part 1
(->>
 (for [line +puzzle+]
   (try
     (prn line)
     (parse line)
     0 ;; ignore retval

     (catch Exception e
       (let [data (ex-data e)]
         (println (.getMessage e))
         (get points (get data :symbol) 0)))))
 (reduce +));; => 392043


;;; part 2
(defn score-closers [closers]
  (let [score {\) 1
               \] 2
               \} 3
               \> 4}]
    (reduce (fn [total val]
              (->> total
                   (* 5)
                   (+ (* (score val))))) 0 closers)))

(defn middle-val [l]
  (nth l (quot (count l) 2)))

(->>
 (for [line +puzzle+]
   (try
     (let [opening-stack (parse line)
           missing (map mapping opening-stack)]
       (println line "- Complete by adding " (apply str missing))
       (score-closers missing))
     (catch Exception _
       nil)))
 (remove nil?)
 (sort)
 (middle-val)
 );; => 1605968119
