(ns day18
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.walk :as walk]
   [clojure.set :as set]))

;[[[[[4,3],5],4],[7,[[8,4],9]]],[1,1]]

(declare printnode pair? left right)

(defrecord SPair [parent left right])
(defrecord SNum [parent value])

(defn printrec [node]
  (walk/prewalk (fn [n]
                  (if (instance? clojure.lang.Ref n)
                    (str "#<Ref@" (format "%x " (System/identityHashCode n)) " " (or (:value @n) "SPair") ">")
                    n)) node))

(defn print-tree
  ([subtree]
   (print-tree subtree ""))
  ([subtree prefix]
   (if (pair? @subtree)
     (do
       (println prefix (str "#<Ref@" (format "%x" (System/identityHashCode subtree)) " SPair" ">"))
       (print-tree (left @subtree) (str "\t" prefix))
       (print-tree (right @subtree) (str "\t" prefix)))
     (println prefix (printrec @subtree)))))

(defmethod clojure.pprint/simple-dispatch SPair
  [^SPair p]
  (prn (printrec p)))

(defmethod clojure.pprint/simple-dispatch SNum
  [^SNum p]
  (prn (printrec p)))

(defn vec->map3
  ([v]
   (vec->map3 v nil))
  ([elt parent]
   (if (vector? elt)
     (let [pair (ref nil)
           left (vec->map3 (first elt)  pair)
           right (vec->map3 (second elt) pair)]
       ;; (prn 'new-pair (str "#<Ref@" (format "%x" (System/identityHashCode pair)) " SPair" ">"))
       (dosync (ref-set pair (SPair. parent left right)))
       pair)
     (ref (SNum. parent  elt)))))

(defn maps->vec [v]
  (cond (pair? @v) [(maps->vec (left v)) (maps->vec (right v))]
        :else (:value @v)))

(defn ref? [node]
  (instance? clojure.lang.Ref node))

(defn nval [node]
  (if (ref? node) @node node))

(defn up [node]
  (-> (nval node) :parent))

(defn left [node]
  (-> (nval node) :left))

(defn right [node]
  (:right (nval node)))

(defn pair? [node]
  (instance? SPair node))

(defn snum+ [snum1 snum2]
  (update snum1 :value + (:value snum2)))

(defn find-left
  "If NODE is right, go up until can go left, then down the right-hand side"
  [node]
  (assert (instance? clojure.lang.Ref node))
  (let [top (loop [cur (up node)
                   seen #{node (up node)}]
              (cond (nil? cur) nil
                    (contains? seen (left cur)) (recur (up cur) (conj seen cur))
                    :else (left cur)))]
    (loop [cur top]
      (if (and cur (pair? @cur))
        (recur (right cur))
        cur))))

(defn find-right
  "If NODE is left, go up until can go right, then down the left-hand side"
  [node]
  (assert (instance? clojure.lang.Ref node))
  (let [top (loop [cur (up node)
                   seen #{node (up node)}]
              (cond (nil? cur) nil
                    (contains? seen (right cur)) (recur (up cur) (conj seen cur))
                    :else (right cur)))]
    (loop [cur top]
      (if (and cur (pair? @cur))
        (recur (left cur))
        cur))))

(defn explode [pair]
  (assert (instance? clojure.lang.Ref pair))
  (assert (instance? SPair @pair))

  (let [left (left pair)
        right (right pair)
        left-num (find-left left)
        right-num (find-right right)
        parent (:parent @pair)]

    (dosync
     (when left-num
       (ref-set left-num (snum+ @left-num @left)))
     (when right-num
       (ref-set right-num (snum+ @right-num @right)))
     (ref-set pair (SNum. parent 0)))))

(defn split [*snum]
  (assert (instance? clojure.lang.Ref *snum))
  (assert (instance? SNum @*snum))

  (let [snum @*snum
        parent (:parent snum)
        val (:value snum)]
    (dosync
     (ref-set *snum
              (SPair. parent
                      (ref (SNum. *snum (int (Math/floor (/ val 2)))))
                      (ref (SNum. *snum (int (Math/ceil (/ val 2))))))))))

(defn snum-seq
  ([snum] (dosync (flatten (snum-seq snum 1))))
  ([snum lvl]
   ;; (prn (printrec snum))
   (alter snum assoc :level lvl)
   (cond (pair? @snum)
         (list snum
               (snum-seq (left snum) (inc lvl))
               (snum-seq (right snum) (inc lvl)))

         :else
         snum)))

(defn explode-all
  "Explode the whole tree"
  [snum-tree]
  (loop [[cur & snums] (snum-seq snum-tree)]
    (cond (and (pair? @cur)
               (>= (:level @cur) 5))
          (do
            (prn 'explode)
            (explode cur)
            (prn (maps->vec snum-tree))
            (recur (snum-seq snum-tree)))
          
          snums
          (recur snums))
    ))

(defn split-one
  "Split the first splittable value. Returns nil when nothing splittable was found"
  [snum-tree]
  (loop [[cur & snums] (snum-seq snum-tree)]
    (cond (and
             (not (pair? @cur))
             (>= (:value @cur) 10))
          (do
            (prn 'split)
            (split cur)
            (prn (maps->vec snum-tree))
            ::split)
          
          snums
          (recur snums))))

(defn reduce-snum [snum-tree]
  (loop []
    ;; we explode all, then we split one. If one got split, explode all again, until we don't split anymore
    (explode-all snum-tree)
    (when (split-one snum-tree)
      (recur)))
  snum-tree)


(defn magnitude [snum]
  (if (pair? @snum)
    (+ (* 3 (magnitude (left snum))) (* 2 (magnitude (right snum))))
    (:value @snum)))

(def spair+ vector)

(def puzzle [
[[[[3,9],[0,5]],[4,6]],3]
[[[8,[3,0]],[[8,4],[9,4]]],[[[0,9],4],[[3,8],2]]]
[3,[3,[[2,8],[1,4]]]]
[8,[[3,6],[[8,9],[4,1]]]]
[[3,[[5,8],[3,3]]],[[[9,3],[6,3]],[[7,0],[8,8]]]]
[[6,[[5,8],7]],[8,[[1,6],7]]]
[[[[8,6],[9,3]],[3,[2,7]]],[[[6,7],[2,8]],[6,7]]]
[[[9,[1,6]],0],[[7,3],[2,4]]]
[[[[4,9],3],6],[[7,5],8]]
[[[[8,3],8],[2,[6,5]]],[[6,[1,9]],[0,2]]]
[[[9,9],[[9,8],1]],[[[7,4],[1,4]],[[1,1],4]]]
[[5,[[8,2],[8,6]]],[9,[7,[8,9]]]]
[[4,6],[8,[3,[1,2]]]]
[[[2,[7,9]],7],[[2,0],[9,2]]]
[[4,9],[[[3,4],[2,9]],5]]
[[[[0,0],[3,7]],[[6,1],8]],[[[4,0],4],8]]
[[4,[[8,9],[2,2]]],[[[1,8],[2,7]],[[6,8],0]]]
[[7,5],[[7,0],1]]
[[[5,[1,0]],1],[[[7,7],[2,2]],[[4,2],8]]]
[[[7,1],[7,3]],[2,0]]
[[[[6,2],3],[3,[5,2]]],[[7,2],[[9,5],[0,1]]]]
[[[[0,3],2],6],9]
[[[9,8],[[7,8],[5,9]]],[[[4,8],[0,2]],[[6,8],[2,3]]]]
[2,[[3,7],9]]
[[[9,9],1],[7,[7,[5,8]]]]
[[8,[1,1]],[8,8]]
[[[[3,3],[1,4]],[[5,3],4]],[5,2]]
[[[[0,9],1],[[3,8],8]],[9,[[8,8],[0,7]]]]
[[[9,4],1],[[9,7],[[6,1],[9,5]]]]
[[[1,[4,0]],9],[[3,7],2]]
[[[5,[0,5]],[5,[9,2]]],[[[2,2],[8,0]],[3,[7,8]]]]
[[[[8,2],3],3],[[[5,4],[0,5]],9]]
[[[3,[6,2]],0],[[[7,3],[6,3]],[[6,3],2]]]
[[6,1],[[[1,2],2],[9,4]]]
[[[1,[9,0]],[[8,2],[4,9]]],[[0,[9,6]],[[0,4],[4,0]]]]
[9,[4,[7,0]]]
[[7,2],[[9,5],8]]
[[6,[[0,6],0]],[[[2,0],[4,1]],[[9,5],4]]]
[[[6,[0,0]],5],[[[5,2],[7,3]],[[2,8],[3,2]]]]
[[[2,7],[[8,2],2]],[[5,[0,6]],[[9,8],[0,4]]]]
[[[8,9],[[4,1],2]],[[[3,4],[4,5]],[[7,4],0]]]
[[5,[2,[2,1]]],[[5,6],[[6,2],[3,0]]]]
[[8,[0,0]],[[6,1],[9,[1,3]]]]
[[[9,[5,8]],5],[[8,[6,6]],[7,5]]]
[3,2]
[[8,[[6,3],[8,4]]],[[2,7],[8,[9,5]]]]
[[[4,[9,1]],[[3,6],[8,8]]],[[[9,0],6],[[3,7],6]]]
[[9,[[4,9],6]],[[8,2],[1,3]]]
[[[2,[4,3]],[[5,6],[7,3]]],7]
[[[[0,1],7],[[9,1],9]],[[[0,1],[6,5]],1]]
[[[7,[5,3]],[[6,6],6]],[[2,7],3]]
[[1,[[5,8],[1,7]]],[[[5,0],[4,7]],[[3,3],[3,7]]]]
[[[[8,8],[2,6]],[1,2]],[[[2,6],4],[1,[1,8]]]]
[5,[[8,[8,2]],0]]
[[6,[[5,9],[8,4]]],[7,[5,9]]]
[[7,3],[[[2,5],4],[[1,1],8]]]
[[[0,1],7],[0,8]]
[[7,[6,6]],[2,9]]
[[[[1,9],1],[[4,8],5]],[[0,[8,3]],[[0,9],[1,5]]]]
[[[0,9],[[6,7],5]],[4,[[1,1],[0,6]]]]
[[[6,1],7],[[[1,4],8],[[9,0],4]]]
[5,[3,[[0,7],[4,9]]]]
[[[[6,0],[1,5]],[[1,5],1]],[[1,[7,1]],[[6,2],7]]]
[[[9,0],8],[[[4,1],[5,4]],[4,[5,1]]]]
[3,[5,9]]
[6,[6,5]]
[[1,[8,0]],[9,0]]
[[[[1,8],3],0],[7,[[0,8],6]]]
[[[[4,2],2],3],[[2,5],[[9,2],4]]]
[[1,[[1,1],[8,4]]],[[[8,1],0],[0,2]]]
[[[[0,7],[8,7]],[9,6]],0]
[[3,7],[[1,[0,9]],[1,[7,6]]]]
[[[[3,5],[4,6]],[[7,1],[8,0]]],6]
[[7,[5,[7,7]]],[4,[5,3]]]
[1,[[[0,0],[4,6]],[7,[1,9]]]]
[[[3,7],[7,[0,6]]],[7,[5,3]]]
[[[[5,3],0],2],[[[2,7],[7,9]],[[1,4],3]]]
[[[[8,3],9],[[8,3],[7,4]]],[[4,[6,0]],[7,[3,7]]]]
[[[6,[5,0]],8],[[[4,5],3],[1,[5,9]]]]
[[7,8],[[6,8],[[8,4],[3,1]]]]
[[[2,7],[6,3]],[[0,0],4]]
[[1,[[6,5],[4,8]]],[[8,[2,7]],[[7,8],[6,8]]]]
[[[2,3],[7,7]],[0,[3,3]]]
[5,[[2,8],[2,[6,9]]]]
[[[[6,3],2],[[2,8],9]],[[[5,6],[8,0]],[[9,3],[5,0]]]]
[[[[6,2],7],[6,1]],[[[5,9],4],4]]
[[[[7,2],[0,4]],[[6,7],7]],[6,[[8,5],[9,0]]]]
[[[[9,6],8],[2,[3,7]]],6]
[[0,[[1,0],4]],[5,[[7,4],[2,4]]]]
[[[[4,4],[4,7]],[[7,4],3]],5]
[[[[8,2],[0,3]],[[7,2],1]],[[7,[1,2]],6]]
[[[3,8],[3,1]],[7,7]]
[[[6,5],[[8,7],4]],3]
[[7,[2,[2,5]]],[9,1]]
[9,2]
[[4,[2,9]],[[4,[2,9]],0]]
[[[0,2],[[2,1],[9,2]]],[[6,[8,2]],[4,[3,8]]]]
[1,[[[2,2],6],[[3,5],6]]]
[[[9,[4,8]],[1,4]],[4,[1,[9,1]]]]
[[[8,0],[[8,4],3]],9]

             ])

;;; part1
(->> puzzle
     (reduce (fn [acc val]
               (-> (spair+ acc val)
                   (vec->map3)
                   (reduce-snum)
                   (maps->vec))))
     vec->map3
     magnitude);; => 3494


;;; part2

(defn pair-magnitudes [s1 s2]
  (-> (spair+ s1 s2)
      (vec->map3)
      (reduce-snum)
      magnitude))

(defn permutations [[head & tail]]
  (when tail
    (concat
     (for [el tail]
       [head el])
     (for [el tail] ; NOT commutative!
       [el head])
     (permutations tail))))

(->> puzzle
     permutations
     (map #(apply pair-magnitudes %))
     (apply max)
     );; => 4712


;;; tests

(assert (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
           (-> [[[[[4,3],4],4],[7,[[8,4],9]]] [1,1]]
               vec->map3
               reduce-snum
               maps->vec)))

(assert (= [[[[5,0],[7,4]],[5,5]],[6,6]]
           (reduce (fn [acc val]
                     (-> (spair+ acc val)
                         (vec->map3)
                         (reduce-snum)
                         (maps->vec)))
                   [[1,1]
                    [2,2]
                    [3,3]
                    [4,4]
                    [5,5]
                    [6,6]])))
