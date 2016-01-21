(ns blx.sortable.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cheshire.core :as json]))

; From Prismatic's plumbing/core
(defmacro fn->
  "Equivalent to `(fn [x] (-> x ~@body))"
  [& body]
  `(fn [x#] (-> x# ~@body)))

(defn parse-json
  "Parses a JSON string, keywordizing keys."
  [s]
  (json/parse-string s true))

(defn read-json-lines
  "Parses each line of the file at `path` as JSON, returning a vector."
  [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
         ; We need to parse non-lazily (mapv), because otherwise with-open will
         ; close the stream before the seq is realized.
         (mapv parse-json))))

(defn map-vals
  "Returns the map k -> (f v) for k -> v in m."
  [f m]
  (persistent!
    (reduce-kv (fn [m k v]
                 (assoc! m k (f v)))
               (transient {})
               m)))

(defn map-id
  "Maps each item in coll into the pair [item (f item)]."
  [f coll]
  (map (juxt identity f) coll))

(defn first-word
  "Returns first word in s, defined by splitting on whitespace."
  [s]
  (-> (or s "")
      (str/split #"\s+" 2)  ; Split at most once
      first))

(defn max-subs
  "Like subs, but doesn't throw if end is longer than s."
  [s start end]
  (subs s start (min end (count s))))

(defn levenshtein
  "Computes the Levenshtein distance between two sequences."
  ; Derived from Yomguithereal/clj-fuzzy/src/clj_fuzzy/levenshtein.cljc
  [c1 c2]
  (let [next-row
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
        (reduce next-row
                (range (inc (count c2)))
                c1))))

(defn str-contains?
  "Returns true if string src contains string what, optionally case-insensitively."
  [^String src ^String what ignore-case?]
  (let [len (count what)]
    (or (zero? len)
        (let [first-char (->> (.charAt what 0)
                              ((juxt #(Character/toLowerCase %)
                                     #(Character/toUpperCase %)))
                              set)]
          (loop [i (- (count src) len)]
            (if (>= i 0)
              (if (first-char (.charAt src i))
                (if (.regionMatches src ignore-case? i what 0 len)
                  true
                  (recur (dec i)))
                (recur (dec i)))
              false))))))
