(ns day3
  (:require [day1 :refer [fetch-input read-lines]]
            [clojure.string :as str]))

(def input ["467..114.."
            "...*......"
            "..35..633."
            "......#..."
            "617*......"
            ".....+.58."
            "..592....."
            "......755."
            "...$.*...."
            ".664.598.."])

(defn x [x]
  (for [[number row :as y] (map-indexed vector x)
        [number character] (map-indexed vector row)]
    []))
(->> ["467..114.."
      "...*......"
      "..35..633."
      "......#..."
      "617*......"
      ".....+.58."
      "..592....."
      "......755."
      "...$.*...."
      ".664.598.."]
     (map-indexed vector)
     (map x)
     #_(for [(map-indexed vector )])
     #_(reduce
      (fn [acc [line input]]
        (for [[col number] (map-indexed vector input)]
          (assoc acc [line col] number))) {}))
(= [1 2] [1 2]) ;; => true
(= [1 2] [2 1]) ;; => false
(+ 467 35 633 617 592 755 664 598)
;; => 4361

(defn dot? [x] (= x \.))

(defn trim [[x y]]
  (cond
    (and (Character/isDigit x) (dot? y)) x ;; keep digit
    (and (dot? x) (Character/isDigit y)) y ;; keep digit
    (and (dot? x) (dot? y)) nil
    (and (dot? x) (not (Character/isDigit y))) y ;; keep symbol
    (and (not (Character/isDigit x)) (dot? y)) x ;; keep symbol
    :else [x y]))

(re-seq #"\d+(\.\d+)*" "..35..633.")
(re-seq #"\d+(\.\d+)*" "467..114..")

(->> #_(apply interleave '((\4 \6 \7 \. \. \1 \1 \4 \. \.) (\. \. \. \* \. \. \. \. \. \.))) ;; correct

#_(apply interleave '((\. \. \. \* \. \. \. \. \. \.) (\. \. \3 \5 \. \. \6 \3 \3 \.))) ;; wrong
#_(apply interleave '((\6 \1 \7 \* \. \. \. \. \. \.) (\. \. \. \. \. \+ \. \5 \8 \.))) ;; wrong
#_(apply interleave '((\. \. \. \. \. \+ \. \5 \8 \.) (\. \. \5 \9 \2 \. \. \. \. \.))) ;; wrong
#_(apply interleave '((\. \. \. \$ \. \* \. \. \. \.) (\. \6 \6 \4 \. \5 \9 \8 \. \.))) ;; correct
     (partition 2)
     (partition 2)
     (map (partial map trim)) ;; remove 114 (number without symbol adjacent to it)
     (map (partial remove nil?)) ;; dots is now nil, remove dots (nils)
     (remove empty?)
     (map flatten)
     (partition 2) ;; join back
     (map (partial mapcat identity))
     (map (partial apply str))
     #_(map flatten))

(let [rows ["467..114.."
            "...*......"
            "..35..633."
            "......#..."
            "617*......"
            ".....+.58."
            "..592....."
            "......755."
            "...$.*...."
            ".664.598.."]]
  (->>
   (for [[y row] (map-indexed vector rows)
         [x character] (map-indexed vector row)]
     [[x y] character])
   (into {})
   (sort)))

(defn pair [xs]
  (->> (apply interleave xs)
       (partition 2)
       (partition 2)
       (map (partial map trim))
       ;; (map (partial remove nil?))
       ;; (remove empty?)
       ;; (map flatten)
       ;; (partition 2)
       ;; (map (partial mapcat identity))
       #_(map (partial apply str))))

(defn contains-symbol [xs]
  (letfn [(pred [x]
            (not (Character/isDigit x)))]
    (some pred xs)))

(defn remove-symbol [xs]
  (letfn [(pred [x]
            (not (Character/isDigit x)))]
    (remove pred xs)))

(->> input
     (map seq)
     (partition 2 1)
     (map pair)
     #_(map (partial filter contains-symbol))
     #_(map flatten)
     #_flatten
     #_(map seq)
     #_(map remove-symbol)
     #_(map (partial apply str))
     #_distinct
     #_(map remove-symbol))

(->> input
     (map  (partial re-seq #"\d+(\.\d+)*"))
     (remove nil?)
     )

(->> input
     (map seq)
     (partition 2 1))

(def syms #"[^\d.]")
(def nums #"\d+")


(def r2 #"(?<=\d)\.|\.(?=\d)|\.(?=[^\d.])") ;; num. | .num | .symbol
(def r4 #"(\d+)[^\d](?!\.)(\d+)")
(def r5 #"(\d+)[^\d](\d+)|(\d+)[^\d]|(\d+)(?!\.)")
(def r6 #"(\d+)[^\d](\d+)|(\d+)[^\d]|[^\d](\d+)(?!\.)")
#_
("467*.114...."     467
 "....3*5...633."   35, 633
 "...35...6#33.."   35, 633
 "617*...#......."
 "617*...+..58."
 "....592+..58.."
 "...592..755."
 "......$..*755.."
 "..66$4*598...")

(defn with-dot? [x]
  (or (nil? x) (str/includes? x ".")))
;; (def r7 #"(?<=\d)\.(?=\d)")
#_(def r7 #"(?<=\d)\.")
(def r7 #"\.(\d+)")
#_
("4.6.7..*..1.1.4....."
 ".....3*5.....6.3.3.."
 "....3.5.....6#3.3..."
 ".6.1.7.*....#......."
 "6.1.7.*....+...5.8.."
 ".....5.9.2+...5.8..."
 "....5.9.2....7.5.5.."
 ".......$...*7.5.5..."
 "...6.6$4..*5.9.8....")

(->> input
     (partition 2 1)
     (map (partial map (partial apply str)))
     (map (partial apply interleave))
     (map (partial apply str))
     ; (map #(str/replace % r7 "")) ;; remove dot only if adjacent to a number
     (map (partial re-seq r5))
     ;; (map (juxt (comp first first) (comp first second)))
     ;; (mapcat (partial remove with-dot?))
     ;; (map (partial re-seq nums))
     #_(map (partial apply str)))

(defn re-pos [re s]
  (if (nil? re)
    {}
    (loop [m (re-matcher re s)
           res {}]
      (if (.find m)
        (recur m (assoc res (.start m) (.group m)))
        res))))
(re-pos #"[^\d.]" ".......*.............*.....814...............$....*........../..94......*....=.............103............/..882*...........+...............")
