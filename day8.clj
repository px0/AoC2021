(ns day8
  (:require utils
            [clojure.string :as str]))

(def +puzzle+-str
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def +puzzle+-str (slurp "day8.input"))
  
(def +puzzle+ (->>
               +puzzle+-str
               str/split-lines
               (map (fn [line]
                      (-> (str/split line #"\| ")
                          second
                          (str/split #" "))))))


;;; part1
(->> +puzzle+
     (mapcat (fn [line]
               (map count line)))
     (filter #{2 ;;1
               4 ;;4
               3 ;;7
               7 ;;8
               })
     (count));; => 521


;;; part2

(defn permutations [s]
  (lazy-seq
   (if (next s)
     (for [head s
           tail (permutations (disj s head))]
       (cons head tail))
     [s])))

(defn lookup-table [uniques]
  (first
   (for [[a b c d e f g] (permutations (set [\a \b \c \d \e \f \g]))
         :let [one #{c f}
               seven #{a c f}
               four #{c b d f}
               eight #{a e c g b d f}
               two #{a c d e g}
               three #{a c d f g}
               five #{a g b d f}
               zero #{a e c g b f}
               six #{a e g b d f}
               nine #{a c g b d f}]
         :when  (= #{zero one two three four five six seven eight nine}
                   (set (map set uniques)))]
     {zero 0,
      one 1,
      two 2,
      three 3,
      four 4,
      five 5,
      six 6,
      seven 7
      eight 8,
      nine 9})))

(defn translate [lookup-key output]
  (utils/parse-int (apply str (map lookup-key (map set output)))))

(def +puzzle+ (->>
               +puzzle+-str
               str/split-lines
               (map (fn [line]
                      (->> (str/split line #" \| ")
                          (map #(str/split % #" ")))))))

(defn part2 [lines]
  (reduce + (map (fn [[uniques output]]
                   (translate (lookup-table uniques) output)) lines)))

(part2 +puzzle+);; => 1016804
