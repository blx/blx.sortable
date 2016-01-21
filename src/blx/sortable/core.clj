(ns blx.sortable.core
  "Solution to the Sortable coding challenge [http://sortable.com/challenge/].

  Matches a collection of third-party product Listings against a collection of
  known products."
  (:require [clojure.java.io :as io] 
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [cheshire.core :as json]
            [tesser.core :as t]
            [taoensso.timbre :refer [info]]
            [taoensso.timbre.profiling :as p]
            [blx.sortable.matcher :as matcher]
            [blx.sortable.util :refer [fn-> parse-json read-json-lines first-word
                                       levenshtein map-vals max-subs]])
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private default-conf
  "Default configuration; overridable via command-line arguments."
  {:products-uri "resources/data/products.txt"
   :listings-uri "resources/data/listings.txt"
   :output-uri   "results.txt"})

; Data model
;
; Input and output are text files with one JSON-encoded object per line,
; but the core matching functions (group-products and match-listings) are
; defined for arbitrary sequences of Product and Listing maps, as defined below.
;
; Product :: {:product_name str
;             :manufacturer str
;             :family str
;             :model str
;             :announced-date str iso-8601}
;
; Listing :: {:title str
;             :manufacturer str
;             :currency str
;             :price str decimal}
;
; Result :: {:product_name str
;            :listings [Listing]}

(defn- family-regex
  [family]
  (when family
    (str "(?:" (str/lower-case family) ")?"
         #"\s*\w*\s*")))

(defn- model-regex
  [model]
  (re-pattern
    (-> (or model "")
        str/lower-case
        ; [_ -] chars are equivalent and optional
        (str/replace #"[\s_-]+" "[\\\\s_-]*")
        ; common model prefixes are optional
        (str/replace #"(dsc|dslr|dmc|pen|slt|is)" "(?:$1)?"))))

(p/defnp product-regex
  "Generates a case-insensitive regex pattern that represents the product."
  [product]
  ; Roughly: word{0,3} family? word? model
  (re-pattern
    (str #"(?i)^(?:[\w()]*\s*){0,3}"
         (family-regex (:family product))
         ; Require some kind of separator before model to keep it distinct,
         ; eg. don't accept "PowerShot SX220 HS" for "Ixus 220 HS".
         #"[\s_-]"
         (or (:model-regex product)
             (model-regex (:model product)))
         ; Don't match if model is directly followed by a number,
         ; eg. don't accept "EOS 100" for "EOS 10".
         #"(?:[^\d]+|$)")))

(defn prepare-product
  "Adds indexing fields to the product."
  [product]
  (-> product
      (assoc :model-regex (model-regex (:model product)))
      (as-> $ (assoc $ :regex (product-regex $)))))

(p/defnp group-products
  "Returns a map of (lowercase first word of manufacturer) -> products."
  [products]
  (->> products
       (group-by (comp first-word
                       (fnil str/lower-case "")
                       :manufacturer))
       (map-vals #(map prepare-product %))))

(p/defnp levenshtein-match
  "Attempts to match candidate to a product manufacturer via shortest Levenshtein
  difference, returning nil if no match."
  [manufacturers manufacturer-first-letters candidate]
  ; This is mainly for things like "OPYMPUS" -> "Olympus"
  (let [min-length 4
        max-length-delta 3
        max-lev-dist 2]

    (and
      ; Very short strings are not likely to be similar enough
      (>= (count candidate) min-length)

      ; Levenshtein is expensive, so require the first letter to match
      ; before bothering to compute it.
      (manufacturer-first-letters (first candidate))

      (p/p :levenshtein
           (when-let [[guess lev-dist]
                      (some->>
                        manufacturers
                        ; Skip Levenshtein if seqs aren't of similar length
                        (filter #(<= (Math/abs (- (count %) (count candidate)))
                                     max-length-delta))
                        (map (juxt identity
                                   (partial levenshtein candidate)))
                        seq
                        (apply min-key second))]
             (when (<= lev-dist max-lev-dist)
               guess))))))

(defn make-manufacturer-matcher
  "Creates a function that, given a listing manufacturer, returns the
  corresponding product manufacturer or nil if no match."
  [products]
  (let [known-manufacturers (keys products)
        first-letters (set (map first known-manufacturers))]
    (fn [_ manufacturer]
      (p/p :manuf-matcher
           (or (some #{(first-word manufacturer)}
                     known-manufacturers)
               (levenshtein-match known-manufacturers
                                  first-letters
                                  manufacturer))))))

(defn make-title-matcher
  "Creates a function that, given a product manufacturer and a listing title,
  returns the corresponding product or nil if no match."
  [products]
  (fn [manufacturer-guess title]
    (p/p :title-matcher
         (->> (products manufacturer-guess)
              ; Check model regex first because it's cheaper than the
              ; full product regex.
              (filter #(and (re-find (:model-regex %) title)
                            (re-find (:regex %) title)))
              first))))

(def listing-match-steps
  "Listing
     -> try to match with a known manufacturer, then
     -> try to match with one of that manufacturer's products"
  [{:accessor (fn-> ((some-fn :manufacturer (comp first-word :title)))
                    str/lower-case
                    (str/replace-first #"^eastman\s*kodak\s*(?:company)?" "kodak")
                    (str/replace-first #"^hewlett\s*packard" "hp"))
    :matcher-maker (comp memoize make-manufacturer-matcher)}

   {:accessor (fn-> :title
                    (max-subs 0 30)
                    str/lower-case
                    (str/replace #"[_-]+" ""))
    :matcher-maker (comp memoize make-title-matcher)}])

(p/defnp match-listings
  "Matches listings to products using listing-matcher. Returns the map
       {:n-listings total-number-of-listings
        :results    [{:product_name ... :listings [...]} ...]}
  The order of the results is nondeterministic because Tesser is used
  to parallelize the matching across all cores."
  [parse-fn listing-matcher listings]
  (let [chunk-size 2048]
    (->> (t/map parse-fn)
         (t/fuse {:n-listings (t/count)
                  :results (->> (t/group-by (comp :product_name listing-matcher))
                                (t/post-combine
                                  ; Discard unmatched listings (nil product_name)
                                  (partial keep (fn [[prod ls]]
                                                  (when prod
                                                    {:product_name prod
                                                     :listings ls}))))
                                (t/into []))})
         (t/tesser (t/chunk chunk-size listings)))))

(defn match! [{:keys [products-uri listings-uri output-uri] :as conf}]
  (info (str "Using conf " conf))
  (let [[n-prods products] (->> products-uri
                                read-json-lines
                                ((juxt count group-products)))
        listing->product (matcher/matcher-for listing-match-steps products)]
    (with-open [listings (io/reader listings-uri)
                out      (io/writer output-uri)]
      (let [{:keys [n-listings results]} (->> (line-seq listings)
                                              (match-listings parse-json listing->product))
            n-matched-prods (count results)
            n-matched-listings (apply + (map (comp count :listings) results))]
        (info (format "Matched %d out of %d listings to %d out of %d products."
                      n-matched-listings n-listings n-matched-prods n-prods))
        (doseq [result-item results]
          (json/generate-stream result-item out)
          (.write out "\n"))))))

(defn- exit [status msg]
  (println msg)
  (System/exit status))

(def ^:private cli-options
  [["-p" "--products PRODUCTS_PATH" "Path to products file"
    :id :products-uri]
   ["-l" "--listings LISTINGS_PATH" "Path to listings file"
    :id :listings-uri]
   ["-o" "--output OUTPUT_PATH" "Path to output file"
    :id :output-uri]
   ["-h" "--help"]])

(defn -main
  [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 summary)
      errors          (exit 1 errors))
    (p/profile
      :info :main
      (match! (merge default-conf options)))
    ; Put pmap to bed
    (shutdown-agents)))
