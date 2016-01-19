(ns blx.sortable.core
  (:require [clojure.java.io :as io] 
            [clojure.string :as str]
            [cheshire.core :as json]
;            [clj-fuzzy.metrics :refer [levenshtein]]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.profiling :as p]
            [blx.sortable.util :refer [first-word levenshtein map-vals]])
  (:gen-class))

(set! *warn-on-reflection* true)
(timbre/set-level! :info)

(def conf
  {:products-uri "resources/data/products.txt"
   :listings-uri "resources/data/listings.txt"
   :out-uri      "results.txt"})

; Product :: {:product_name str
;             :manufacturer str
;             :family str
;             :model str
;             :announced-date str iso-8601}

; Listing :: {:title str
;             :manufacturer str
;             :currency str
;             :price str decimal}

; Result :: {:product_name str
;            :listings [Listing]}

(defn- parse-json
  "Parses a JSON string, keywordizing keys."
  [s]
  (json/parse-string s true))

(p/defnp load-input
  "Parses each line of the file at `path` as a JSON object, returning a vector."
  [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
         ; We need to parse non-lazily (mapv), because otherwise with-open will
         ; close the stream before the seq is realized.
         (mapv parse-json))))

(p/defnp product-regex
  "Generates a case-insensitive regex pattern that represents the product."
  [product]
  (let [fmt-model (fn [s]
                    (-> (or s "")
                        ; [_ -] chars are equivalent and optional
                        (str/replace #"[\s_-]+" "[\\\\s_-]*")
                        ; common model prefixes are optional
                        (str/replace #"(?i)(DSC|DSLR|DMC|PEN|SLT|IS)" "(?:$1)?")
                        str/lower-case))]
    ; Roughly: word{0,3} family? word? model
    (re-pattern
      (str #"^(?:[\w()]*\s*){0,3}"
           (when-let [family (:family product)]
             (str "(?:" (str/lower-case family) ")?"
                  #"\s*\w*\s*"))
           (fmt-model (:model product))
           ; Don't match if model is directly followed by a number,
           ; eg. don't accept "EOS 100" for "EOS 10".
           #"(?:[^\d]+|$)"))))

(defn prepare-product [product]
  (assoc product :regex (product-regex product)))

(p/defnp group-products
  "Returns a map of (lowercase first word of manufacturer) to products."
  [products]
  (->> products
       (pmap prepare-product)
       (group-by (comp first-word
                       (fnil str/lower-case "")
                       :manufacturer))))

(def listing-preprocessors
  {:manufacturer (fn [listing]
                   (when-let [manuf (or (:manufacturer listing)
                                        (first-word (:title listing)))]
                     (-> manuf
                         str/lower-case
                         (str/replace-first #"^hewlett\s*packard" "hp"))))
   :title        (fn [listing]
                   (when-let [title (:title listing)]
                     (-> title
                         (#(subs % 0 (min 30 (count %))))
                         str/lower-case
                         (str/replace #"[_-]+" ""))))})

(defn make-title-matcher
  [products]
  (memoize
    (fn [title]
      (p/p :title-matcher
           (first
             (filter #(re-find (:regex %) title)
                     products))))))

(defn make-manufacturer-matcher
  [products matchers]
  (memoize
    (->> matchers
         (map #(partial % (keys products)))
         (apply some-fn))))

(def manufacturer-matchers
  [(fn first-word? [manufacturers candidate]
     (p/p :firstword
          (some #{(first-word candidate)} manufacturers)))
   
   (fn levenshtein? [manufacturers candidate]
     ; This is mainly for things like "OPYMPUS" -> "Olympus"
     (p/p :levenshtein
          (let [min-length 4        ; Very short strings are not likely to be similar
                max-length-delta 3  ; Skip Levenshtein if seqs aren't similar length
                max-lev-dist 2]
            (and
              (>= (count candidate) min-length)

              ; Levenshtein is expensive, so require the first letter to match
              ; before trying to compute it.
              ((set (map first manufacturers)) (first candidate))

              (when-let [levs
                         (p/p :lev-lev
                              (->> manufacturers
                                   (filter #(< (Math/abs (- (count %) (count candidate)))
                                               max-length-delta))
                                   (map (juxt identity
                                              (partial levenshtein candidate)))
                                   seq))]
                (let [[guess lev-dist] (apply min-key second levs)]
                  (when (<= lev-dist max-lev-dist)
                    guess)))))))])

(p/defnp match-listing
  "Attempts to match listing to a product in products, returning a vector
  [matching-product listing] if successful, else [nil listing]."
  [manufacturer-matcher title-matchers listing]
  (let [product-guess 
        (when-let [manufacturer-guess
                   (some->> listing
                            ((:manufacturer listing-preprocessors))
                            manufacturer-matcher)]
          (some->> listing
                   ((:title listing-preprocessors))
                   ((title-matchers manufacturer-guess))))]
    [product-guess listing]))

(p/defnp match-all
  "Returns the sequence of Result maps produced by matching the
  listings to the products."
  [products listings]
  (let [manufacturer-matcher (make-manufacturer-matcher products manufacturer-matchers)
        title-matchers (map-vals make-title-matcher products)]
    (pmap #(match-listing manufacturer-matcher title-matchers %)
          listings)))

(p/defnp group-matches [matches]
  (let [product first
        listing second]
    (->> matches
         (group-by product)
         (map (fn [[prod matching-listings]]
                (if prod
                  {:product_name (:product_name prod)
                   :listings (map listing matching-listings)}
                  matching-listings))))))

(defn -main
  [& args]
  ; We need to read all products into memory at once, but we
  ; can process listings lazily / line-by-line.
  (p/profile :info :main
  (let [products (-> (:products-uri conf)
                     load-input
                     group-products)]
    (with-open [listings (io/reader (:listings-uri conf))
                out      (io/writer (:out-uri conf))]
      (doseq [result-item (->> (line-seq listings)
                               (map parse-json)
                               (match-all products)
                               group-matches)]
        (json/generate-stream result-item out)
        (.write out "\n")))))
  ; Put pmap to bed
  (shutdown-agents))
