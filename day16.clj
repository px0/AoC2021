(ns day16
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.walk :as walk]
   [clojure.set :as set]))

(declare parse-packets)

(defn hex->bin [hx]
  (.toString (BigInteger. hx 16) 2))

(def packet-type {4 ::literal})

(defn bin->int [b]
  (Integer/parseInt (apply str b) 2))

(defn parse-pkg-version [p b]
  [(assoc p :version (bin->int (take 3 b))) (drop 3 b)])

(defn parse-pkg-type [p b]
  [(assoc p :type (get packet-type (bin->int (take 3 b)) ::operator)) (drop 3 b)])

(defn parser-combinator [fns b]
  (loop [[fn & fns] fns
         b b
         p {}]
    (if fn
      (let [[new-p rest-b] (fn p b)]
            (prn p b '-> fn)
        (recur fns rest-b new-p))
      [p b])))

(defmulti parse-content (fn [p _]
                          (:type p)))

(defmethod parse-content ::literal
  [p b]
  (loop [b b
         num []]
    (let [[continue? & bits] (take 5 b)]
      (if (= \1 continue?)
        (recur (drop 5 b) (apply conj num bits))
        [(assoc p :value (bin->int (apply conj num bits))) (drop 5 b)]))))

   
(defmethod parse-content ::operator
  [p b]
  (let [[length-type & b] b
        length-num (if (= length-type \1) 11 15)
        [length-bits b] (split-at length-num b)
        length (bin->int length-bits)
        subpackets-bits (take length b)]
    [(assoc p :children (parse-packets b))
     b])
  )
          

(defn parse-packet [b]
  (parser-combinator [parse-pkg-version
                      parse-pkg-type
                      parse-content]
                     b))

(defn parse-packets [b]
  (loop [packets []
         b b]
    (prn 'parse-packets 'packets packets 'b b 'etmpy? (empty? b))
    (if-not (empty? b)
      (let [[new-p rem-b] (parse-packet b)]
        (recur (conj packets new-p) rem-b))
      packets)))


         
(-> (hex->bin "EE00D40C823060")
    parse-packet)

(parse-packet (hex->bin "38006F45291200"))
