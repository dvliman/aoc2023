(ns day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def seed->soil {79 81 14 14 55 57 13 13})

(def example
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
")

(defn numbers [s]
  (map parse-long (re-seq #"\d+" s)))

(defn x [dest src n]
  (->> (interleave (range src (+ src n))
                   (range dest (+ dest n)))
       (partition 2)
       (map vec)
       (into {})))

#_(def input (line-seq (io/reader (io/resource "day5.txt"))))
#_(let [#_#_input (line-seq (io/reader (io/resource "day5.txt")))
      head  (first input)
      tail  (rest input)]
  (->> tail
       (filter (complement empty?))
       (partition-by #(Character/isLetter (first %)))
       #_#_#_#_#_
       (map-indexed vector)
       (remove (comp even? first))
       (map second)
       (map (partial map (comp (partial apply x) numbers)))
       (map (partial apply merge))
       ))

#_(let [input () #_(str/split example #"\n")
      head  (first input)
      tail  (rest input)

      seeds (numbers head)
      mappings
      (->> tail
           (filter (complement empty?))
           (partition-by #(Character/isLetter (first %)))
           (map-indexed vector)
           (remove (comp even? first))
           (map second)
           (map (partial map (comp (partial apply x) numbers)))
           (map (partial apply merge)))

      mappings
      (cons seed->soil
            mappings)]

  (apply min (reduce
        (fn [acc seed]
          (conj
           acc
           (reduce (fn [intermediate m]
                     (prn "m:" (sort m))
                     (prn "k:" intermediate)
                     (prn "v:" (get m intermediate intermediate))
                     (get m intermediate intermediate)) seed mappings)))
        []
        seeds)))



#_(with-open [rdr (io/reader (io/resource "day5.txt"))]
  (->> (line-seq rdr)))
