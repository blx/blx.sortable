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

(defn- parse-json [s]
  (json/parse-string s true))

(defn load-input [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
         (mapv parse-json))))

(defn remove-chars [s]
  (-> s
      ((fnil str/replace "") #"[ _-]+" "")
      str/lower-case))

(defn prepare-product [product]
  (-> product
      (update :product_name remove-chars)
      (update :model remove-chars)))


(defn common-prefix-length
  "Returns the length of the longest common prefix between a and b."
  [a b]
  (reduce (fn [match-len [a b]]
            (if (= a b)
              (inc match-len)
              (reduced match-len)))
          0
          (map vector a b)))



(defn match-manufacturer [mproducts listing]
  (->> (keys mproducts)
       (map remove-chars)
       (some #(when (.startsWith (remove-chars (:manufacturer listing)) %)
                %))))

(defn match-title
  "Attempts to match listing to a product in products, returning a vector
  [score matching-product] if successful, else nil."
  [products listing]
        ; Longer than just manufacturer name
  (let [min-match-len (max 3 (count (:manufacturer listing)))
        matches (->> products
                     (map #(vector (common-prefix-length (:title listing)
                                                         (:product_name %))
                                   %))
                     (filter (fn [[score prod]]
                               (and (> score min-match-len)
                                    (if-let [model (:model prod)]
                                      (.contains (:title listing) model)
                                      true))))
                     (sort-by first >))]
    ; Do not allow ties
    (when (and matches
               (not= (first (first matches))
                     (first (second matches))))
      (first matches))))

(defn match [products listing]
  (let [listing' (update listing :title remove-chars)]
    (when-let [[_ match] (match-title products listing')]
      [listing match])))

(defn domatch
  "Returns the sequence of Result maps produced by matching the
  listings to the products."
  [products listings]
  (let [matches (->> listings
                     (pmap #(match products %))  ; pmap was a 4x speedup
                     (filter some?))]
    (->> matches
         (group-by second)
         (map (fn [[prod matching-listings]]
                {:product_name (:product_name prod)
                 :listings (map first matching-listings)})))))



(defn test-random [products listings]
  ((juxt #(match products %) identity)
   (rand-nth listings)))



(defn -main
  [& args]
  ; We need to read all products into memory, but we
  ; can process listings lazily / line-by-line.
  (let [products (mapv prepare-product (load-input (:products-uri conf)))]
    (with-open [listings (io/reader (:listings-uri conf))
                out (io/writer (:out-uri conf))]
      (doseq [item (->> (line-seq listings)
                        (map parse-json)
                        (domatch products))]
        (json/generate-stream item out)
        (.write out "\n")))))
