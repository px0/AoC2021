(ns day16
  (:require
   [clojure.edn :as edn]
   [clojure.test :as test]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.walk :as walk]
   [clojure.set :as set]))

(declare parse-packets)

(defn hex->bin [hx]
  (let [bin (.toString (BigInteger. hx 16) 2)
        bin-len (count bin)
        leading-zeroes (if (zero? (mod bin-len 8))
                         0
                         (- 8 (mod bin-len 8)))]
    (prn bin bin-len leading-zeroes)
    ( str (apply str (repeat leading-zeroes "0")) bin)))

(def packet-type {4 ::literal})

(defn bin->int [b]
  (BigInteger. (apply str b) 2))


(defn bpeek [b n]
  (take n @b))

(defn bpop! [b n]
  (let [rez (bpeek b n)]
    (swap! b (partial drop n))
    rez))

(defn bempty? [b]
  (empty? @b))

(defn parse-pkg-version [b p]
  (assoc p :version (bin->int (bpop! b 3))))

(defn parse-pkg-type [b p]
  (let [type-val (bin->int (bpop! b 3))]
        (assoc p
               :type-val type-val
               :type (get packet-type type-val ::operator))))

(defmulti parse-content (fn [_ p]
                          (:type p)))

(defmethod parse-content ::literal
  [b p]
  (prn 'literal)
  (loop [num []]
    (prn 'num num)
    (let [[continue? & bits] (bpop! b 5)
          num (apply conj num bits)]
      (if (= \1 continue?)
        (recur num)
        (assoc p :value (bin->int num))))))

   
(defmethod parse-content ::operator
  [b p]
  (prn 'operator)
  (let [length-type (first (bpop! b 1))
        ;; If the length type ID is 0, then the next 15 bits are a
        ;; number that represents the total length in bits of the
        ;; sub-packets contained by this packet. If the length type ID
        ;; is 1, then the next 11 bits are a number that represents
        ;; the number of sub-packets immediately contained by this
        ;; packet.
        length-bits-num (if (= length-type \0) 15 11)
        length-bits (bpop! b length-bits-num)
        _ (prn 'lb length-bits)
        length (bin->int length-bits)
        ]
    (if (= length-type \0) ;; total length in bits. \1 is num of subpackets
    (assoc p :children (parse-packets (atom (bpop! b length))))
    (assoc p :children (parse-packets b length)))
    ))

(defn parse-packet [b]
  (def theb b)
  (-> {}
      ((partial parse-pkg-version b))
      ((partial parse-pkg-type b))
      (#(do (prn %) %))
      ((partial parse-content b))))

(defn parse-packets
  ([b n]
   (loop [packets [] 
          n n]
     (prn 'parse-packets 'packets packets 'n n)
     (if (zero? n)
       packets
       (recur (conj packets (parse-packet b)) (dec n)))))
  
  ([b]
   (loop [packets []]
     (prn 'parse-packets 'packets packets)
     (if-not (bempty? b)
       (recur (conj packets (parse-packet b)))
       packets))))

         
(defn sum-versions [p]
  (->> (tree-seq :children :children p)
       (map :version)
       (reduce +)))

;;; part 1
(sum-versions
(parse-packet (atom (seq (hex->bin "805311100469800804A3E488ACC0B10055D8009548874F65665AD42F60073E7338E7E5C538D820114AEA1A19927797976F8F43CD7354D66747B3005B401397C6CBA2FCEEE7AACDECC017938B3F802E000854488F70FC401F8BD09E199005B3600BCBFEEE12FFBB84FC8466B515E92B79B1003C797AEBAF53917E99FF2E953D0D284359CA0CB80193D12B3005B4017968D77EB224B46BBF591E7BEBD2FA00100622B4ED64773D0CF7816600B68020000874718E715C0010D8AF1E61CC946FB99FC2C20098275EBC0109FA14CAEDC20EB8033389531AAB14C72162492DE33AE0118012C05EEB801C0054F880102007A01192C040E100ED20035DA8018402BE20099A0020CB801AE0049801E800DD10021E4002DC7D30046C0160004323E42C8EA200DC5A87D06250C50015097FB2CFC93A101006F532EB600849634912799EF7BF609270D0802B59876F004246941091A5040402C9BD4DF654967BFDE4A6432769CED4EC3C4F04C000A895B8E98013246A6016CB3CCC94C9144A03CFAB9002033E7B24A24016DD802933AFAE48EAA3335A632013BC401D8850863A8803D1C61447A00042E3647B83F313674009E6533E158C3351F94C9902803D35C869865D564690103004E74CB001F39BEFFAAD37DFF558C012D005A5A9E851D25F76DD88A5F4BC600ACB6E1322B004E5FE1F2FF0E3005EC017969EB7AE4D1A53D07B918C0B1802F088B2C810326215CCBB6BC140C0149EE87780233E0D298C33B008C52763C9C94BF8DC886504E1ECD4E75C7E4EA00284180371362C44320043E2EC258F24008747785D10C001039F80644F201217401500043A2244B8D200085C3F8690BA78F08018394079A7A996D200806647A49E249C675C0802609D66B004658BA7F1562500366279CCBEB2600ACCA6D802C00085C658BD1DC401A8EB136100")))));; => 971N

;;; tests

(test/deftest unittests
  (test/is (= (parse-pkg-version (atom (seq "100")) {})
              {:version 4}))

  (test/is (= (parse-pkg-type (atom (seq "100")) {})
              {:type-val 4, :type :day16/literal}))

  (test/is (= (parse-pkg-type (atom (seq "10")) {})
              {:type-val 2, :type ::operator}))
  (test/is (= (hex->bin "D2FE28")
              "110100101111111000101000"))

    (test/is (= (hex->bin "38006F45291200")
                "00111000000000000110111101000101001010010001001000000000"))

  (test/is (= (bin->int "11001001100011001111001011001010")
              3381457610))
  )

(test/deftest integration-tests

  ;; literal value with binary representation 011111100101, which is 2021 in decimal
  (test/is (= (parse-packet (atom (seq (hex->bin "D2FE28"))))
              {:version 6, :type-val 4, :type :day16/literal, :value 2021}))

  ;; operator packet (hexadecimal string 38006F45291200) with length type ID 0 that contains two sub-packets
    (test/is (= (parse-packet (atom (seq (hex->bin "38006F45291200"))))
                {:version 1,
                 :type-val 6,
                 :type :day16/operator,
                 :children
                 [{:version 6, :type-val 4, :type :day16/literal, :value 10}
                  {:version 2, :type-val 4, :type :day16/literal, :value 20}]}))

  (test/is (= (parse-packet (atom (seq (hex->bin "EE00D40C823060"))))
              {:version 7,
               :type-val 3,
               :type :day16/operator,
               :children
               [{:version 2, :type-val 4, :type :day16/literal, :value 1}
                {:version 4, :type-val 4, :type :day16/literal, :value 2}
                {:version 1, :type-val 4, :type :day16/literal, :value 3}]}
              ))

  ;;8A004A801A8002F478 represents an operator packet (version 4) which
  ;;contains an operator packet (version 1) which contains an operator
  ;;packet (version 5) which contains a literal value (version 6);
  ;;this packet has a version sum of 16.
  
  (test/is (= (parse-packet (atom (seq (hex->bin "8A004A801A8002F478"))))
              {:version 4,
               :type-val 2,
               :type :day16/operator,
               :children
               [{:version 1,
                 :type-val 2,
                 :type :day16/operator,
                 :children
                 [{:version 5,
                   :type-val 2,
                   :type :day16/operator,
                   :children
                   [{:version 6, :type-val 4, :type :day16/literal, :value 15}]}]}]}
              ))
  
  (test/is (= 16
              (sum-versions
              {:version 4,
               :type-val 2,
               :type :day16/operator,
               :children
               [{:version 1,
                 :type-val 2,
                 :type :day16/operator,
                 :children
                 [{:version 5,
                   :type-val 2,
                   :type :day16/operator,
                   :children
                   [{:version 6, :type-val 4, :type :day16/literal, :value 15}]}]}]}
              )))
  )

