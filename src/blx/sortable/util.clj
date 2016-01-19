(ns blx.sortable.util
  (:require [clojure.string :as str]))

(defn first-word
  "Returns first word in s, defined by splitting on whitespace. Not lazy."
  ; For large strings (10000 chars or more), the lazy version using partition-by
  ; is faster.
  [s]
  (-> (or s "")
      (str/split #"\s+" 2)  ; Split at most once
      first))

(defn levenshtein
  "Computes the Levenshtein distance between two sequences."
  ; Derived from Yomguithereal/clj-fuzzy/src/clj_fuzzy/levenshtein.cljc
  [c1 c2]
  (let [lev-next-row
        (fn [previous current]
            (reduce
              (fn [row [diagonal above other]]
                (let [update-val (if (= other current)
                                   diagonal
                                   (inc (min diagonal above (peek row))))]
                  (conj row update-val)))
              [(inc (first previous))]
              (map vector previous (next previous) c2)))]
      (peek
        (reduce lev-next-row
                (range (inc (count c2)))
                c1))))

(defn str-contains?
  [^String src ^String what ignore-case?]
  (let [len (count what)]
    (or (zero? len)
        (let [first-char (.charAt what 0)
              first-lo (Character/toLowerCase first-char)
              first-up (Character/toUpperCase first-char)]
          (loop [i (- (count src) len)]
            (if (>= i 0)
              (if (not= (.charAt src i)
                        first-lo
                        first-up)
                (recur (dec i))
                (if (.regionMatches src ignore-case? i what 0 len)
                  true
                  (recur (dec i))))
              false))))))

