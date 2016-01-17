(ns blx.sortable
  (:require [clojure.java.io :as io] 
            [clojure.string :as str]
            [cheshire.core :as json]
            [clj-fuzzy.metrics :refer [levenshtein]]
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
         ; We need to return a vector (mapv), because otherwise with-open will
         ; close the stream before the lazy sequence is realized.
         (mapv parse-json))))

(defn product-regex
  "Generates a regex pattern that represents the product."
  [product]
  (let [optional-spaces (fn [s]
                          (-> (or s "")
                              (str/replace #"[\s_-]+" "[\\\\s_-]*")
                              (str/replace #"(?i)(DSC|DSLR|DMC|PEN|SLT|IS)" "(?:$1)?")))]
    (re-pattern
      (str #"(?i)^(?:[\w()]*\s*){0,3}"
           (if-let [fam (:family product)]
             (str "(?:"
                  (:family product)
                  ")?"
                  #"\s*\w*\s*"))
           (optional-spaces (:model product))
           #"[^\d]+"))))

(defn prepare-product [product]
  (-> product
      (#(assoc % :regex (product-regex %)))))

(defn common-prefix-length
  "Returns the length of the longest common prefix between sequences a and b."
  [a b]
  (->> (map = a b)
       (take-while true?)
       count))

(defn starts-with? [^String s candidate]
  (.startsWith s (or candidate "")))

(defn first-word [s]
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


(def manufacturer-matchers
  [(fn first-word? [manufacturers candidate]
     (some #{(first-word candidate)} manufacturers))
   
   (fn levenshtein? [manufacturers candidate]
     ; This is mainly for things like "OPYMPUS" -> "Olympus"
     (p/p :levenshtein
     (and (> (count candidate) 3)  ; Very short strings are not likely to be similar
          (let [max-delta 2
                [guess lev-dist] (->> manufacturers
                                      (map (juxt identity (partial levenshtein candidate)))
                                      (apply min-key second))]
            (when (<= lev-dist max-delta)
              guess)))))])

(def match-manufacturer
  (memoize
    (fn [manufacturers listing-manufacturer]
      (let [min-prefix-length 2
            preprocessor #(str/replace-first % #"^(?i)Hewlett\s*Packard" "hp")]
        (when-let [listing-manufacturer (and (>= (count listing-manufacturer)
                                                 min-prefix-length)
                                             (preprocessor listing-manufacturer))]
          (let [match (->> manufacturer-matchers
                           ; TODO cache this once we've calculated manufacturers once
                           (map #(partial % manufacturers))
                           (apply some-fn))]
            (->> listing-manufacturer
                 str/lower-case
                 match)))))))


(defn match-title
  "Attempts to match listing's title to a product in products, returning the
  matched product if successful, else nil."
  [products listing]
    (when-let [title (-> (or (:title listing) "")
                         (str/replace #"[_-]+" ""))]
    (->> products
         (filter #(re-find (:regex %) title))
         first)))


(p/defnp match-listing
  "Attempts to match listing to a product in products, returning a vector
  [matching-product listing] if successful, else nil."
  [products listing]
  (when-let [manufacturer-guess
             (p/p :match-manuf (match-manufacturer (keys products)
                                 (or (:manufacturer listing)
                                     (first-word (:title listing)))))]
    (if-let [product-guess (p/p :match-title (match-title (products manufacturer-guess)
                                        listing))]
      [product-guess listing]
      [nil listing])))


(p/defnp match-all
  "Returns the sequence of Result maps produced by matching the
  listings to the products."
  ; We take in all the listings, as opposed to doing (pmap match-listing listings),
  ; because we need to group the results by Product after matching.
  [products listings]
  (let [matches (->> listings
                     (pmap #(match-listing products %))  ; pmap was a 4x speedup
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




(defn test-random [products listings]
  (let [listing (rand-nth listings)]
    (or (match-title products listing)
        [nil listing])))



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
