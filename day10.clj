(ns day10
  (:require utils
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

(defn parse
  ([[next-sym & rest-syms]]
   (parse rest-syms next-sym []))

  ([[next-sym & rest-syms] sym stack]

   (let [expected-closing (mapping sym)
         openings (set (keys mapping))]
;;     (prn 'sym sym  'expc expected-closing 'next next-sym '& rest-syms)

     (cond
       (contains? openings next-sym)
       (->
        (parse rest-syms next-sym (conj stack sym))
        (parse sym stack))

       (= expected-closing next-sym)
       (if (and (empty? stack)
                (empty? rest-syms))
         :success
         (if (empty? stack)
           (parse rest-syms)
           rest-syms))

       (empty? rest-syms)
       (throw (ex-info "Unfinished" {:type :unfinished}))

       :else
       (do
         (throw (ex-info (str "Expected " expected-closing " from " sym " but got " next-sym " instead")
                         {:type :error
                          :symbol next-sym}))
         rest-syms)))))


;;; part 1
(->>
 (for [line +puzzle+]
   (try
     (prn line)
     (prn 'line (parse line))

     (catch Exception e
       (let [data (ex-data e)]
         (prn 'exc data)
         (get points (get data :symbol) 0)))))
 (reduce +));; => 392043


;;; part 2

(defn parse
  ([[next-sym & rest-syms]]
   (parse rest-syms next-sym []))

  ([[next-sym & rest-syms] sym stack]

   (let [expected-closing (mapping sym)
         openings (set (keys mapping))]
;;     (prn 'sym sym  'expc expected-closing 'next next-sym '& rest-syms)

     (cond
       (contains? openings next-sym)
       (->
        (parse rest-syms next-sym (conj stack sym))
        (parse sym stack))

       (= expected-closing next-sym)
       (if (and (empty? stack)
                (empty? rest-syms))
         :success
         (if (empty? stack)
           (parse rest-syms)
           rest-syms))

       (empty? rest-syms)
       

       :else
       (do
         (throw (ex-info (str "Expected " expected-closing " from " sym " but got " next-sym " instead")
                         {:type :error
                          :symbol next-sym}))
         rest-syms)))))
