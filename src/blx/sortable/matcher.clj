(ns blx.sortable.matcher
  "Matching engine

  Abstractly, the matching engine is a function (a -> Maybe b) that operates
  in the lexical context of a collection of b.
  
  We represent the a->b? matching process as a vector of steps that produce
  and refine the match. A step is a map with `:accessor` and `matcher-maker`
  keys.
  
  Each step is executed in order like this:

      (when-let [field (accessor listing)]
        (matcher result-of-prev-matcher field))

  until either a step returns a falsy result or all steps have been run, at
  which point the final result will be returned.
  
  Before using the pipeline, it should be prepared by

      (load-source pipeline source-data)

  which will generate `:matcher` functions for each step by calling the step's
  `:matcher-maker` with `source-data`. This factory approach lets matchers
  store some preprocessed state based on the source data.")

(defn match
  [match-pipeline item]
  (reduce (fn [result {:keys [accessor matcher]}]
            (if-let [field (and (some? result)
                                (accessor item))]
              (matcher result field)
              (reduced nil)))
          false
          match-pipeline))

(defn load-source
  [match-pipeline source]
  (->> match-pipeline
       (map #(assoc % :matcher ((:matcher-maker %) source)))))
