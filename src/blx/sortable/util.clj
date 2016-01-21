(ns blx.sortable.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cheshire.core :as json]))

(defn update*
  "Like update, but the update-fn receives the entire map instead of just
  the value at k.

  Example: `(update* {:a 1} :b (comp inc :a)) -> {:a 1 :b 2}`."
  [m k f]
  (assoc m k (f m)))

(defn parse-json
  "Parses a JSON string, keywordizing keys."
  [s]
  (json/parse-string s true))

(defn read-json-lines
  "Parses each line of the file at path as JSON, returning a vector."
  [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
         ; We need to parse non-lazily (mapv), because otherwise with-open will
         ; close the stream before the seq is realized.
         (mapv parse-json))))

(defn write-json-lines
  "Encodes each item in coll as JSON and writes the results linewise
  to the file at path."
  [path coll]
  (with-open [out (io/writer path)]
    (doseq [item coll]
      (json/generate-stream item out)
      (.write out "\n"))))

(defn first-word
  "Returns first word in s, defined by splitting on whitespace."
  [s]
  (-> (or s "")
      (str/split #"\s+" 2)  ; Split at most once (return <= 2 pieces)
      first))

(defn max-subs
  "Like subs, but doesn't throw if end is longer than s."
  [s start end]
  (subs s start (min end (count s))))

(defn levenshtein
  "Computes the Levenshtein distance between two sequences."
  ; Derived from Yomguithereal/clj-fuzzy/src/clj_fuzzy/levenshtein.cljc
  [s1 s2]
  (let [next-row
        (fn [previous current]
          (reduce
            (fn [row [diagonal above other]]
              (let [update-val (if (= other current)
                                 diagonal
                                 (inc (min diagonal above (peek row))))]
                (conj row update-val)))
            [(inc (first previous))]
            (map vector previous (next previous) s2)))]
    (peek
      (reduce next-row
              (range (inc (count s2)))
              s1))))
