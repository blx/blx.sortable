(ns blx.sortable
  (:require [clojure.java.io :as io] 
            [clojure.string :as str]
            [cheshire.core :as json]
;            [clj-fuzzy.metrics :refer [levenshtein]]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.profiling :as p])
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



; TODO
;
; LISTING title      vs    PRODUCT model
; x Panasonic Lumix DMC-FZ40 (DMC prefix not in title)
; x Panasonic Lumix ZS10 (vs product fam:"Lumix" + "DMC-DZ10")
; x Canon PowerShot A3300 (vs product "A3300 IS")
; x Kodak EasyShare Z1485 (vs product "Z1485 IS")
; x Olympus E-P2 (vs product "PEN E-P2")
; ~ Sony A390 (vs product "DSLR-A390")
; x Sony A230L (vs product "DSLR-A290")
; x Sony Alpha DSLR-SLT-A55 (vs product "SLT-A55")


(defn- parse-json
  "Parses a JSON string, keywordizing keys."
  [s]
  (json/parse-string s true))

(defn load-input
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
                        (str/replace #"(?i)(DSC|DSLR|DMC|PEN|SLT|IS)" "(?:$1)?")))]
    (re-pattern
      ; Roughly: word{0,3} family? word? model
      (str #"(?i)^(?:[\w()]*\s*){0,3}"
           (when-let [family (:family product)]
             (str "(?:" family ")?"
                  #"\s*\w*\s*"))
           (fmt-model (:model product))
           ; Don't match if model is directly followed by a number, eg.
           ; don't accept "EOS 100" for "EOS 10".
           #"(?:[^\d]+|$)"))))

(defn prepare-product [product]
  (assoc product :regex (product-regex product)))

(defn first-word
  "Returns first word in s, defined by splitting on whitespace. Not lazy."
  ; For large strings (10000 chars or more), the lazy version using partition-by
  ; is faster.
  [s]
  (-> (or s "")
      (str/split #"\s+" 2)  ; Split at most once
      first))

(defn group-products
  [products]
  (->> products
       (map prepare-product)
       (group-by (comp first-word
                       (fnil str/lower-case "")
                       :manufacturer))))

(defn levenshtein
  "Computes the Levenshtein distance between two sequences."
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

(def manufacturer-matchers
  [(fn first-word? [manufacturers candidate]
     (p/p :firstword
          (some #{(first-word candidate)} manufacturers)))
   
   (fn levenshtein? [manufacturers candidate]
     ; This is mainly for things like "OPYMPUS" -> "Olympus"
     (p/p :levenshtein
          (let [min-length 4
                max-length-delta 3  ; Skip Levenshtein if seq length > 3
                max-lev-dist 2]
            (and
              ; Very short strings are not likely to be similar
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

(def match-manufacturer
  (memoize
    (fn [manufacturer-matcher listing-manufacturer]
      (let [preprocessor (comp str/lower-case
                               #(str/replace-first % #"^(?i)Hewlett\s*Packard" "hp"))]
        (when-let [listing-manufacturer (preprocessor listing-manufacturer)]
          (manufacturer-matcher listing-manufacturer))))))


(defn match-title
  "Attempts to match listing's title to a product in products, returning the
  matched product if successful, else nil."
  [products title]
    (when-let [title (-> (or title "")
                         (str/replace #"[_-]+" ""))]
      (p/p :mt-filter
           (->> products
                (filter #(re-find (:regex %) title))
                first))))


(p/defnp match-listing
  "Attempts to match listing to a product in products, returning a vector
  [matching-product listing] if successful, else nil."
  [products manufacturer-matcher listing]
  (when-let [manufacturer-guess
             (p/p :match-manuf (match-manufacturer manufacturer-matcher
                                                   (or (:manufacturer listing)
                                                       (first-word (:title listing)))))]
    (if-let [product-guess
             (p/p :match-title (match-title (products manufacturer-guess)
                                            (:title listing)))]
      [product-guess listing]
      [nil listing])))


(p/defnp match-all
  "Returns the sequence of Result maps produced by matching the
  listings to the products."
  ; We take in all the listings, as opposed to doing (pmap match-listing listings),
  ; because we need to group the results by Product after matching.
  [products listings]
  (let [manufacturer-matcher (->> manufacturer-matchers
                                  (map #(partial % (keys products)))
                                  (apply some-fn))
        matches (->> listings
                     (pmap #(match-listing products manufacturer-matcher %))  ; pmap was a 4x speedup
                     )
;                     (filter some?))
        product first
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
                               (match-all products))]
        (json/generate-stream result-item out)
        (.write out "\n")))))
  ; Put pmap to bed
  (shutdown-agents))
