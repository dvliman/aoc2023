(ns day5
  (:require [day1 :refer [fetch-input read-lines]]
            [clojure.string :as str]))

(def seed->soil {79 81 14 14 55 13})


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

(let [input (str/split example #"\n")
      head  (first input)
      tail  (rest input)

      mappings
      (->> tail
           (filter (complement empty?))
           (partition-by #(Character/isLetter (first %)))
           (map-indexed vector)
           (remove (comp even? first))
           (map second)
           (map (juxt
                 (comp (partial apply x) numbers first)
                 (comp (partial apply x) numbers second)))
           (map (partial apply merge)))

      mappings
      (cons seed->soil
            mappings)]
  (numbers head))
