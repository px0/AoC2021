(ns utils
  (:require [clojure.string :as str]))

(defn parse-int [int-str]
  (Integer/parseInt int-str))

(defn split-commas-to-ints [line]
	(for [num (str/split (str/replace line #"\s" "") #",")]
		(parse-int num)))
