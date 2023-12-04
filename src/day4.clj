(ns day4
  (:require [day1 :refer [fetch-input read-lines]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def split-bar #"(\d+(?:\s+\d+)*)(?:\s*\|\s*)(\d+(?:\s+\d+)*)")

(defn numbers [s]
  (into #{} (map parse-long (str/split s #"\s+"))))

(defn worth [n]
  (last (take n (iterate (partial * 2) 1))))

(->> (fetch-input 4)
     read-lines
     (map (partial re-seq split-bar))
     (map first)
     (map (juxt (comp numbers second) (comp numbers last)))
     (map (partial apply set/intersection))
     (map (comp worth count))
     (remove nil?)
     (reduce +))
;; => 25571

(def input
  [
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
"Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
"Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
"Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
"Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
"Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
   ])

(defn card-number [s]
  (->> (re-seq #"^Card\s+(\d+):" s)
       first last
       parse-long))

(defn subsequent-card-nums [[card-number how-many]]
  (cons card-number (take how-many (iterate inc (inc card-number)))))

(defn debug [path x] (prn path x) x)

(defn x [acc line] ;; returns card and copies with their count
  (let [matching-numbers
        (->> line
             (re-seq split-bar)
             first
             ((juxt (comp numbers second) (comp numbers last)))
             (apply set/intersection)
             count)
        card     (card-number line)
        winnings (subsequent-card-nums [card matching-numbers])]
    [card (into {} (map vec (partition 2 (interleave winnings (repeat (get acc card 1))))))]))


(->>
 (fetch-input 4)
 read-lines
 ;; input
 (reduce
  (fn [acc line]
    (let [[_ copies-counts] (x acc line)]
      (merge-with + acc copies-counts)))
  {})
 sort
 #_vals
 #_(reduce +))
;; => 17611239
;; => 8707706
(- 8805731 8707706);; => 98025
