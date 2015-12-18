(ns blx.sortable
  (:require [cheshire.core :as json])
  (:gen-class))

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

(defn load-json [path]
  (-> path
      clojure.java.io/reader
      (json/parse-stream true)))

(defn match-products
  [products listings]
  ...)





(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [products (load-json "resources/data/products.txt")
        listings (load-json "resources/data/listings.txt")
        out "results.json"]
    (-> (match-products products listings)
        (json/generate-stream (clojure.java.io/writer out)))))
