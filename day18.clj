(ns day18
  (:require [utils]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.set :as set]))

;[[[[[4,3],5],4],[7,[[8,4],9]]],[1,1]]



(def x [[6,[5,[4,[3,2]]]],1])



(defn vec->map
  ([v] (vec->map v 1))
  ([v lvl]
  (mapcat (fn [elt]
         (cond (vector? elt)
               (vec->map elt (inc lvl))

               :else
               [{:val elt :lvl lvl}]
               ))
       v)))

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
