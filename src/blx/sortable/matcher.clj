(ns blx.sortable.matcher
  "Matching engine

  The matching engine is a function (a -> Maybe b) that operates
  in the lexical context of a collection of b.
  
  For extensibility, we represent this `item -> Maybe source-item` matching process
  as a vector of match-steps that produce and refine the match. A step is a map
  with the following keys:

    - `:accessor` is a function `item -> Maybe d` that is called on the item to
      retrieve and preprocess whatever data is used by the matcher.
    - `:matcher-maker` is a function `[source-item] -> (r, d -> Maybe r)` that,
      when called with the collection of source items to be matched against,
      returns a matcher function that, given the result of the previous matcher
      and the data returned by `:accessor`, returns the refined result or `nil`.

      This matcher function is generated and attached to the `:matcher` key of each
      product by calling `(matcher-for match-steps source-data)` once the source
      data are available, which returns the final, fully composed
      `item -> Maybe source-item` matching function. This factory approach lets
      matchers store some preprocessed state based on the source data.
  
  To match an item to a source-item, each step is executed in order like this:

      (when-let [field (accessor item)]
        (matcher result-of-prev-matcher field))

  until either a step's matcher returns a falsy result or all steps have been run,
  at which point the final result will be returned."
  (:require [blx.sortable.util :refer [update*]]))

(defn match
  [match-steps item]
  (reduce (fn [result {:keys [accessor matcher]}]
            (if-let [field (and (some? result)
                                (accessor item))]
              (matcher result field)
              (reduced nil)))
          false
          match-steps))

(defn matcher-for
  "Creates a matcher function for source-items."
  [match-steps source-items]
  (let [apply-items #(% source-items)
        steps (mapv #(update* % :matcher (comp apply-items :matcher-maker))
                    match-steps)]
    (fn [item]
      (match steps item))))
