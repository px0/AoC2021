(ns day12
  (:require [utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn generate-connections-map [conns]
  (reduce (fn [acc [A B]]
            (-> acc
             (update-in [A] (fnil conj #{}) B)
             (update-in [B] (fnil conj #{}) A)
             )) {} conns))

(defn puzzle [s]
  (->> s
       (str/split-lines)
       (map #(str/split % #"-"))
       (generate-connections-map)))


(defn small-cave? [cave]
  (= (str/lower-case cave) cave))

(def big-cave? (complement small-cave?))
  
(defn search-paths
  ([caves]
   (let [*found-paths (atom #{})]
     (search-paths caves "start" [] *found-paths)
     @*found-paths))

  ([caves curpos curpath *found-paths]
   (if (= curpos "end")
     (when (not (contains? @*found-paths (conj curpath "end")))
       (prn 'found curpath)
       (swap! *found-paths conj curpath))
     (doseq [next-cave (caves curpos)
            :when (or (big-cave? next-cave)
                      (not (contains? (set curpath) next-cave)))]
        (search-paths caves next-cave (conj curpath curpos) *found-paths)))))

;;; part1
(count (search-paths (puzzle "he-JK
wy-KY
pc-XC
vt-wy
LJ-vt
wy-end
wy-JK
end-LJ
start-he
JK-end
pc-wy
LJ-pc
at-pc
xf-XC
XC-he
pc-JK
vt-XC
at-he
pc-he
start-at
start-XC
at-LJ
vt-JK")));; => 4104


;;; part 2

(defn allow-visit? [cave path]
  (or (big-cave? cave)
      (not (contains? (set path) cave))
      (and (not= cave "start")
           (not (some #{2} (vals (frequencies (filter small-cave? path))))))))

(defn search-paths2
  ([caves]
   (let [*found-paths (atom #{})]
     (search-paths2 caves "start" [] *found-paths)
     @*found-paths))

  ([caves curpos curpath *found-paths]
   (if (= curpos "end")
     (when (not (contains? @*found-paths (conj curpath "end")))
       (prn 'found (conj curpath "end"))
       (swap! *found-paths conj (conj curpath "end")))
     (doseq [next-cave (caves curpos)
             :when (allow-visit? next-cave (conj curpath curpos))]
       (search-paths2 caves
                      next-cave
                      (conj curpath curpos)
                      *found-paths)))))

(count (search-paths2 (puzzle "he-JK
wy-KY
pc-XC
vt-wy
LJ-vt
wy-end
wy-JK
end-LJ
start-he
JK-end
pc-wy
LJ-pc
at-pc
xf-XC
XC-he
pc-JK
vt-XC
at-he
pc-he
start-at
start-XC
at-LJ
vt-JK")));; => 119760
