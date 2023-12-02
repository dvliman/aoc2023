(ns day2
  (:require [day1 :refer [fetch-input read-lines]]))

(defn game-id [s]
  (->> (re-seq #"^Game\s+(\d+):" s)
       first last
       parse-long))

(defn cubes [s]
  (map (juxt (comp parse-long second) last) (re-seq #"(\d+)(\s+)(\w+)" s)))

(defn possible [[_ cubes]]
  (let [target {"red" 12 "green" 13 "blue" 14}]
    (every? (fn [[count color]]
              (<= count (target color))) cubes)))

(->> (fetch-input 2)
     read-lines
     (map (juxt game-id cubes))
     (filter possible)
     (map first)
     (reduce +))
;; => 2331

(defn powerset [[_ cubes]]
  (->> (for [[_ vs] (group-by second cubes)]
         (apply max-key first vs))
       (map first)
       (apply *)))

(->> (fetch-input 2)
     read-lines
     (map (juxt game-id cubes))
     (map powerset)
     (reduce +))
;; => 71585
