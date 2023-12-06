(ns day5
  (:require [clojure.java.io :as io]))

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

;; this approach is too slow; the input file may range from 2 mil to 300 mil and more
(defn x [dest src n]
  (->> (interleave (range src (+ src n))
                   (range dest (+ dest n)))
       (partition 2)
       (map vec)
       (into {})))

(defn next-value [current m]
  (let [found (filter (fn [[_ src n]]
                        (<= src current (+ src n))) m)]
    (if-let [[dest src _] (first found)]
      (+ dest (- current src))
      current)))

(let [input #_(str/split example #"\n")
      (line-seq (io/reader (io/resource "day5.txt")))

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
           (map (partial map (comp numbers))))]
  (->> seeds
       (map #(reduce next-value % mappings))
       (apply min)))
;; => 214922730
