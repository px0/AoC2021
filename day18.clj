(ns day18
  (:require 
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]
            [clojure.set :as set]))

;[[[[[4,3],5],4],[7,[[8,4],9]]],[1,1]]

(declare printnode)

(def x [[6,[5,[4,[3,2]]]],1])
(def x [[[[1 2] [3 4]] [5 [7 8]]] 9])



(defrecord SPair [parent level left right])
(defrecord SNum [parent value])

(defn printrec [node]
   (walk/prewalk (fn [n]
              (if (instance? clojure.lang.Ref n)
                (str "#<Ref " (or (:value @n) "SPair") ">")
                n)) node))

(defmethod clojure.pprint/simple-dispatch SPair 
          [^SPair p]
  (prn (printrec p)))

(defmethod clojure.pprint/simple-dispatch SNum 
          [^SNum p]
            (prn (printrec p)))

(defn vec->map3
  ([v]
   (vec->map3 v 1 nil))
  ([elt lvl parent ]
   (if (vector? elt)
     (let [pair (ref nil)
           left (vec->map3 (first elt) (inc lvl) pair )
           right (vec->map3 (second elt) (inc lvl) pair )]
       (dosync (ref-set pair (SPair. parent lvl left right))))
     (ref (SNum. parent elt)))))


(defn vec->map2
  ([v]
   (let [lst (atom [])
         tree (vec->map2 v 1 nil lst nil)]
     #_{:list @lst
        :tree tree}
     tree))
  ([elt lvl parent *elt-list orientation]
   (if (vector? elt)
     (let [pair (ref nil)
           left (vec->map2 (first elt) (inc lvl) pair *elt-list :left)
           right (vec->map2 (second elt) (inc lvl) pair *elt-list :right)]
       (dosync (ref-set pair {:parent parent
                              :type :pair
                              :lvl lvl
                              :left left
                              :orientation orientation
                              :right right})))

     (let [node (ref {:type :const
                      :val elt
                      :parent parent
                      :orientation orientation})]
       (swap! *elt-list conj node)
       node))))

(defn printnode [node]
   (walk/prewalk (fn [n]
              (if (instance? clojure.lang.Ref n)
                (str "#<Ref " (or (:val @n) (:type @n)) ">")
                n)) node))

(def pn printnode)

(defn ref? [node]
  (instance? clojure.lang.Ref node))

(defn nval [node]
  (if (ref? node) @node node))

(defn orientation [node] 
  (-> (nval node) :orientation))

(defn up [node]
  (-> (nval node) :parent))

(defn left [node]
  (-> (nval node) :left))

(defn right [node]
  (:right (nval node)))

(defn pair? [node]
  (= :pair (:type (nval node))))

;;; TODO
;; defrecord for sn numbers
;; install pprinter

    
(defn find-left
  "If NODE is right, go up until can go left, then down the right-hand side"
  [node]
  (let [top (loop [cur (up node)
                   seen #{node}]
              (cond (nil? cur) nil
                    (contains? seen (left cur)) (recur (up cur) (conj seen cur))
                    :else (left cur)))]
    (loop [cur top]
      (if (pair? cur)
        (recur (right cur))
        cur))))

;;; buggy
(defn find-right
  "If NODE is left, go up until can go right, then down the left-hand side"
  [node]
  (let [top (loop [cur (up node)
                   seen #{node}]
              (prn 'up (pn cur))
              (cond (nil? cur) nil
                    (contains? seen (right cur)) (recur (up cur) (conj seen cur))
                    :else (right cur)))]
    (prn 'right)
    (loop [cur top]
      (if (pair? cur)
        (do
        (prn 'left (pn cur))
        (recur (left cur)))
        cur))))
  
  
(defn find-num [node dir]
  (loop [cur (-> (:parent @node))]
    (cond (nil? @cur) nil
          (= :const (:type @(dir @cur))) @(dir @cur)
          ( (get @cur dir)) (recur cur)
          ))
          
          





(defn vec->map
  ([v] (vec->map v 1))

  ([v lvl ]
  (mapcat (fn [elt]
            (cond (vector? elt)
               (vec->map elt (inc lvl ))

               :else
               [{:val elt :lvl lvl }]
               ))
          v)))


(defn vec->map
  ([v] (vec->map v 1 (atom {}) nil))
   ([elt lvl acc parent]
    (if (vector? elt)
      (let [id (java.util.UUID/randomUUID)]
         {:parent parent
          :type :pair
          :id id
          :lvl lvl
          :left (vec->map (first elt) (inc lvl) acc id  )
         :right (vec->map (second elt) (inc lvl) acc id )})

      {:type :const :val elt :parent parent})))

(comment "index by this?"
         (tree-seq #(= :pair (:type %)) (juxt :left :right) t))

(defn vec->map
  ([v] (vec->map v 1 (atom -1)))
  ([elt lvl idx]
   (if (sequential? elt)
     (mapv #(vec->map % (inc lvl) idx) elt)
     {:val elt
      :lvl lvl
      :id (java.util.UUID/randomUUID)
      :idx (swap! idx inc)})))

(defn v+ [v1 v2]
  (cond
    (and v1 v2)
    {:val (+ (:val v1) (:val v2)) :lvl (:lvl v1)}

    v1 v1

    v2 nil
    :else nil 
    ))

(declare explode1 split1)

(defn explode1
  ([acc vrest]
   (cond

     (> (:lvl (first vrest)) 4)
     (let [before (last acc)
           acc (vec (butlast acc))
           [v1 v2 nxt & vrest] vrest]
       ;; (println 'explode [before v1 v2 nxt])
       (prn 'explode!)

       [[] ; empty acc -> start again
        (remove nil? ;can happen when exploding
                (concat (conj acc
                              (v+ before v1)
                              {:val 0 :lvl (dec (:lvl v1))}
                              (v+ nxt v2))
                        vrest))
        explode1 ; next step try this again
        ;; prn
        ])

     :else
     [acc ;only split advances the pointer
      vrest
      split1])))
  
(defn split1 [acc vrest]
  (cond

    (>= (:val (first vrest)) 10)
    (let [[{:keys [lvl val]} & vrest] vrest]
      (prn 'split!)
      [
       [] ;empty acc
       (concat (conj acc
                     {:val (int (Math/floor (/ val 2))) :lvl (inc lvl)}
                     {:val (int (Math/ceil (/ val 2))) :lvl (inc lvl)})
               vrest)
       explode1
       ])

    :else
    [
     (conj acc (first vrest))
     (next vrest)
     explode1
     ]))

(def maps->vec #(map :val %))

(defn maps->vec [maps]
  (loop [acc ""
         [{:keys [lvl val] :as mhead} & mrest :as maps] maps
         level 1]
  (cond
    (nil? mhead)
    (edn/read-string (str acc \]))

    (> lvl level)
    (recur (str acc \[) maps (inc level))

    (< lvl level)
    (recur (str acc \]) maps (dec level))

    (= lvl level)
    (recur (str acc val " ") mrest level))))

  
(defn mv2 [maps]
  (prn maps)
  (mapv (fn [m]
         (if (coll? m)
           
           m)) maps))
  
(def x
  [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])

alter

(def r (ref 23))
