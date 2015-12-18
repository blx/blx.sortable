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

(defn remove-special-chars [s]
  ((fnil str/replace "") s #"[_-]" ""))

(defn remove-spaces [s]
  ((fnil str/replace "") s #" " ""))

(defn prepare-product [product]
  (-> product
      (update :product_name (comp remove-spaces remove-special-chars))))



(defn longest-starting-match [a b]
  (->> [a b]
       (map (comp remove-spaces remove-special-chars))
       (map str/lower-case)
       (apply map vector)
       (reduce (fn [match-len [a b]]
                 (if (= a b)
                   (inc match-len)
                   (reduced match-len)))
               0)))




(defn match-manufacturer [mproducts listing]
  (->> (keys mproducts)
       (some #(when (.startsWith (:manufacturer listing) %)
                %))))

(defn match-title [products listing]
  (let [min-match-len 3
        matches (->> products
                     (map #(vector (longest-starting-match (:title listing)
                                                           (:product_name %))
                                   %))
                     (sort-by first >)
                     (filter (fn [[_ prod]]
                               (if (:model prod)
                                 (.contains (remove-special-chars (remove-spaces (:title listing)))
                                            (remove-special-chars (remove-spaces (:model prod))))
                                 true)))
                     (filter #(> (first %)
                                 ; Longer than just manufacturer name
                                 (max min-match-len (count (:manufacturer listing)))))
                     (take 3))]
    ; Do not allow ties
    (when (and matches
               (not= (first (first matches))
                     (first (second matches))))
      (first matches))))

(defn match' [products listing]
  (println "Trying to match " listing)
  (let [mproducts (group-by :manufacturer products)
        est-manufacturer (match-manufacturer mproducts listing)
        est-products (match-title products listing)]
    [est-manufacturer est-products]))


(def match-threshold 20)

(def match-preds
  [[(fn [products listing]
      (some #(.startsWith (:manufacturer listing) %)
            (map :manufacturer products)))
    20]])


(defn match-product
  "Returns the product matched by listing, or nil if no match."
  [products listing]
  (let [score-seq (map (fn [[pred weight]]
                         (when (pred listing)
                           weight))
                       match-preds)]
    (reduce (fn [score x]
              (let [score' (+ score x)]
                (if (>= score' match-threshold)
                  (reduced score')
                  score')))
            0 score-seq)))



(defn -main
  [& args]
  ; We need to read all products into memory, but we
  ; can process listings lazily / line-by-line.
  (let [products (mapv prepare-product (load-input (:products-uri conf)))]
    (with-open [listings (io/reader (:listings-uri conf))
                out (io/writer (:out-uri conf))]
      (doseq [result (->> (line-seq listings)
                          (map parse-json)
                          (map #(match-product products %)))]
        (json/generate-stream result out)))))
