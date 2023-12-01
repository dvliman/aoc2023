(ns day1
  (:require [clj-http.client :as http]
            [clojure.string :as str]))

(def session-token "53616c7465645f5fd1fe6ed0eb22e7b424aa0d01c4da41e898c5e0ef95d5bdda96d7de1061751bacca11cf41633b1a10da28dc0d3f3d58d75e04030461209b40")
(def with-auth
  {:headers {"Cookie" (str "session=" session-token)}})

(defn fetch-input [day]
  (:body
   (http/get
    (format "https://adventofcode.com/2023/day/%s/input" day)
    with-auth)))

(defn read-lines [xs]
  (str/split xs (re-pattern (System/getProperty "line.separator"))))

(defn is-digit [x]
  (Character/isDigit x))

(->> (fetch-input 1)
     read-lines
     (map (fn [calibration]
            (let [digits (filter is-digit (seq calibration))]
              (Integer/parseInt (apply str [(first digits) (last digits)])))))
     (reduce +))
;; => 56465

(defn convert [x]
  (condp = x
    "one" 1
    "two" 2
    "three" 3
    "four" 4
    "five" 5
    "six" 6
    "seven" 7
    "eight" 8
    "nine" 9
    (parse-long x)))

(def pattern #"(?=(one|two|three|four|five|six|seven|eight|nine|ten|\d))")

(defn detect [calibration]
  (map (comp convert second) (re-seq pattern calibration)))

(->> (fetch-input 1)
     read-lines
     (map (fn [calibration]
            (let [digits (detect calibration)]
              (Integer/parseInt (apply str [(first digits) (last digits)])))))
     (reduce +))
;; => 55902
