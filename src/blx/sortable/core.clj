(ns blx.sortable.core
  "Solution to the Sortable coding challenge [http://sortable.com/challenge/].

  Matches a collection of third-party product Listings against a collection of
  known products."
  (:require [clojure.java.io :as io] 
            [clojure.string :as str]
            [cheshire.core :as json]
            [tesser.core :as t]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.profiling :as p]
            [blx.sortable.matcher :as matcher]
            [blx.sortable.util :refer [fn-> parse-json read-json-lines first-word
                                       levenshtein map-vals max-subs]])
  (:gen-class))

(set! *warn-on-reflection* true)
(timbre/set-level! :info)

(def conf
  {:products-uri "resources/data/products.txt"
   :listings-uri "resources/data/listings.txt"
   :out-uri      "results.txt"})

; Data model
;
; Input and output are text files with one JSON-encoded object per line,
; but the core matching functions (group-products and match-listings) are
; defined for arbitrary sequences of maps.
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

(defn family-regex
  [family]
  (when family
    (str "(?:" (str/lower-case family) ")?"
         #"\s*\w*\s*")))

(defn model-regex
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
  [family-regex product]
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

(defn prepare-products
  "Adds indexing fields to each product."
  [products]
  ; This is a little convoluted, but requiring product-regex to take the
  ; family-regex-generating function lets us pass in a memoized version,
  ; while ensuring that the memo storage is freed when we're done processing.
  (let [family-regex (memoize family-regex)]
    (->> products
         (map #(as-> % prod
                 (assoc prod :model-regex (model-regex (:model prod)))
                 (assoc prod :regex (product-regex family-regex prod)))))))

(p/defnp group-products
  "Returns a map of (lowercase first word of manufacturer) -> products."
  [products]
  (->> products
       (group-by (comp first-word
                       (fnil str/lower-case "")
                       :manufacturer))
       (map-vals prepare-products)))

(p/defnp levenshtein-match?
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
  (let [manufacturers (keys products)
        first-letters (set (map first manufacturers))]
    (fn [_ candidate]
      (p/p :manuf-matcher
           (or (some #{(first-word candidate)} manufacturers)
               (levenshtein-match? manufacturers first-letters candidate))))))

(defn make-title-matcher
  "Creates a function that, given a product manufacturer and a listing title,
  returns the corresponding product or nil if no match."
  [products]
  (fn [manufacturer-guess title]
    (p/p :title-matcher
         (->> (products manufacturer-guess)
              (filter #(and (re-find (:model-regex %) title)
                            (re-find (:regex %) title)))
              first))))

(def listing-matcher
  "Listing
     -> try to match with a known Manufacturer, then
     -> try to match with one of that Manufacturer's Products"
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

(p/defnp match-listings [prepare-fn listing-matcher listings]
  (let [chunk-size 2048]
    (->> (t/map prepare-fn)
         (t/group-by (comp :product_name listing-matcher))
         (t/post-combine
           (partial map (fn [[prod ls]]
                          {:product_name prod
                           :listings ls})))
         (t/into [])
         (t/tesser (t/chunk chunk-size listings)))))

(defn -main
  [& args]
  (p/profile
    :info :main
    (let [listing->product (->> (:products-uri conf)
                                read-json-lines
                                group-products
                                (matcher/load-source listing-matcher)
                                (partial matcher/match))]
      (with-open [listings (io/reader (:listings-uri conf))
                  out      (io/writer (:out-uri conf))]
        (let [results (->> (line-seq listings)
                           (match-listings parse-json listing->product))]
          (doseq [result-item results]
            (json/generate-stream result-item out)
            (.write out "\n"))))))
  ; Put pmap to bed
  (shutdown-agents))
