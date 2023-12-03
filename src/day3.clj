(ns day3
  (:require [day1 :refer [fetch-input read-lines]]
            [clojure.string :as str]))

(->> (fetch-input 3)
    read-lines
(take 2))

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
(apply interleave '((\. \. \. \* \. \. \. \. \. \.) (\. \. \3 \5 \. \. \6 \3 \3 \.))) ;; wrong
#_(apply interleave '((\6 \1 \7 \* \. \. \. \. \. \.) (\. \. \. \. \. \+ \. \5 \8 \.))) ;; wrong
#_(apply interleave '((\. \. \. \. \. \+ \. \5 \8 \.) (\. \. \5 \9 \2 \. \. \. \. \.))) ;; wrong
#_(apply interleave '((\. \. \. \$ \. \* \. \. \. \.) (\. \6 \6 \4 \. \5 \9 \8 \. \.))) ;; correct
     (partition 2)
     #_(partition 2)
     #_(map (partial map trim)) ;; remove 114 (number without symbol adjacent to it)
     ;; (map (partial remove nil?)) ;; dots is now nil, remove dots (nils)
     ;; (remove empty?)
     ;; (map flatten)
     ;; (partition 2) ;; join back
     ;; (map (partial mapcat identity))
     ;; (map (partial apply str))
     #_(map flatten))


(defn pair [xs]
  (->> (apply interleave xs)
       (partition 2)
       (partition 2)
       (map (partial map trim))
       (map (partial remove nil?))
       (remove empty?)
       (map flatten)
       (partition 2)
       (map (partial mapcat identity))
       (map (partial apply str))))

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
     (map (partial filter contains-symbol))
     (map flatten)
     flatten
     (map seq)
     (map remove-symbol)
     (map (partial apply str))
     distinct
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

#_
((["467*" nil nil "467" nil] ["114." nil nil "114" nil])
 (["3*5" "3" "5" nil nil] ["633." nil nil "633" nil])
 (["35." nil nil "35" nil] ["6#33" "6" "33" nil nil])
 (["617*" nil nil "617" nil])
 (["617*" nil nil "617" nil] ["58." nil nil "58" nil])
 (["592+" nil nil "592" nil] ["58." nil nil "58" nil])
 (["592." nil nil "592" nil] ["755." nil nil "755" nil])
 (["755." nil nil "755" nil])
 (["66$4" "66" "4" nil nil] ["598." nil nil "598" nil]))



(defn with-dot? [x]
  (or (nil? x) (str/includes? x ".")))

(->> input
     (map seq)
     (partition 2 1)
     (map (partial map (partial apply str)))
     (map (partial apply interleave))
     (map (partial apply str))
     (map #(str/replace % r2 ""))
     (map (partial re-seq r6)) ;; not quite the regex I want so processing below
     ;; (map (juxt (comp first first) (comp first second)))
     ;; (mapcat (partial remove with-dot?))
     ;; (map (partial re-seq nums))
     #_(map (partial apply str)))
