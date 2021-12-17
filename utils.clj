(ns utils
  (:require [clojure.string :as str]))

(defn parse-int [int-str]
  (Integer/parseInt int-str))

(defn split-commas-to-ints [line]
	(for [num (str/split (str/replace line #"\s" "") #",")]
		(parse-int num)))

(def contains-not? (complement contains?))

(defn includes? [lst elt]
  (some #{elt} lst))

(def ANSI-CODES
  {:reset              "[0m"
   :bright             "[1m"
   :blink-slow         "[5m"
   :underline          "[4m"
   :underline-off      "[24m"
   :inverse            "[7m"
   :inverse-off        "[27m"
   :strikethrough      "[9m"
   :strikethrough-off  "[29m"

   :default "[39m"
   :white   "[37m"
   :black   "[30m"
   :red     "[31m"
   :green   "[32m"
   :blue    "[34m"
   :yellow  "[33m"
   :magenta "[35m"
   :cyan    "[36m"

   :bg-default "[49m"
   :bg-white   "[47m"
   :bg-black   "[40m"
   :bg-red     "[41m"
   :bg-green   "[42m"
   :bg-blue    "[44m"
   :bg-yellow  "[43m"
   :bg-magenta "[45m"
   :bg-cyan    "[46m"
   })

(defn ansi
  [code]
  (str \u001b (get ANSI-CODES code (:reset ANSI-CODES))))

(defn with-ansi [code s]
  (str (ansi code) s (ansi :reset)))
