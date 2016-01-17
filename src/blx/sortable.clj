(ns blx.sortable
  (:require [clojure.java.io :as io] 
            [clojure.string :as str]
            [cheshire.core :as json])
  (:gen-class))

(def conf
  {:products-uri "resources/data/products.txt"
   :listings-uri "resources/data/listings.txt"
   :out-uri "results.json"})

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

(defn load-input
  "Parses the file at `path` as a vector of JSON objects."
  [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
         ; We need to return a vector (mapv), because otherwise with-open will
         ; close the stream before the lazy sequence is realized.
         (mapv parse-json))))

(defn normalize-str [s]
  (-> (or s "")
      (str/replace #"[ _-]+" "")
      str/lower-case))

(defn prepare-product [product]
  (-> product
      (update :product_name normalize-str)
      (update :model normalize-str)))

(defn common-prefix-length
  "Returns the length of the longest common prefix between sequences a and b."
  [a b]
  (->> (map = a b)
       (take-while true?)
       count))

(comment
  (defn match-manufacturer [mproducts listing]
    (->> (keys mproducts)
         (map remove-chars)
         (some #(when (.startsWith (normalize-str (:manufacturer listing)) %)
                  %))))
)

(defn match-title
  "Attempts to match listing to a product in products, returning a vector
  [listing matching-product] if successful, else nil."
  [products listing]
  (let [listing-title (normalize-str (:title listing))
        ; Longer than just manufacturer name
        min-match-len (max 3 (count (:manufacturer listing)))
                     ; This is fairly fast. Combining the map and filter
                     ; as one reduce into a (transient []) didn't help much
                     ; and was harder to read.
        matches (->> products
                     (map #(vector (common-prefix-length listing-title
                                                         (:product_name %))
                                   %))
                     (filter (fn [[score prod]]
                               (and (> score min-match-len)
                                    (if-let [model (:model prod)]
                                      (.contains listing-title model)
                                      true))))
                     (sort-by first >))]
    ; Do not allow ties
    (when (and matches
               (not= (first (first matches))
                     (first (second matches))))
      (let [[_ match] (first matches)]
       [listing match]))))

(defn match-all
  "Returns the sequence of Result maps produced by matching the
  listings to the products."
  ; We take in all the listings, as opposed to doing (pmap match-listing listings),
  ; because we need to group the results by Product after matching.
  [products listings]
  (let [matches (->> listings
                     (pmap #(match-title products %))  ; pmap was a 4x speedup
                     (filter some?))]
    (->> matches
         (group-by second)
         (map (fn [[prod matching-listings]]
                {:product_name (:product_name prod)
                 :listings (map first matching-listings)})))))



(defn test-random [products listings]
  (let [listing (rand-nth listings)]
    (or (match-title products listing)
        [nil listing])))



(defn -main
  [& args]
  ; We need to read all products into memory at once, but we
  ; can process listings lazily / line-by-line.
  (let [products (map prepare-product (load-input (:products-uri conf)))]
    (with-open [listings (io/reader (:listings-uri conf))
                out      (io/writer (:out-uri conf))]
      (doseq [item (->> (line-seq listings)
                        (map parse-json)
                        (match-all products))]
        (json/generate-stream item out)
        (.write out "\n"))))
  ; Put pmap to bed
  (shutdown-agents))
