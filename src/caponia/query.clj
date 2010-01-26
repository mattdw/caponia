(ns caponia.query
  (:use caponia.stemmer))

(defn query
  "Retrieve all matches for query-string's stems from index."
  [index query-string]
  (let [stems (tokenise query-string)]
    (into {}
          (for [stem stems]
            [stem (get @index stem)]))))

(defn merge-and
  "Process query results by AND; only return [id weights] which
  matched all stems."
  [query-results]
  (let [ids (set (mapcat (comp keys second) query-results))
        all-vals (vals query-results)]
    (into {}
      (filter #(> (second %) 0)
              (for [id ids]
                [id (apply *
                           (for [result-set all-vals]
                             (get result-set id 0)))])))))

(defn merge-or
  "Get all matched [id weights], where weight is the sum of
  all occurrences."
  [query-results]
  (apply merge-with + (vals query-results)))

(defn do-search
  "Do a search, merging results as indicated by merge-style.
  Defaults to :and."
  ([index term]
   (do-search index term :and))
  ([index term merge-style]
   (let [merge-func (if (= merge-style :and) merge-and merge-or)]
     (reverse (sort-by second (merge-func (query index term)))))))
