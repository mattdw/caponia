(ns caponia.index
  (:use
     [stemmers.core :only [stems]]
     clojure.contrib.duck-streams
     clojure.contrib.str-utils
     clojure.contrib.seq-utils))

(defn make-index
  "Create a new (empty) index."
  []
  (agent {}))

;; ## Functions that take stems

(defn add-entry
  "Insert an entry into the index, associating {key weight} with stem"
  [index stem key weight]
  (send index assoc-in [stem key] weight))

(defn remove-entries
  "dissociate key from stems"
  [index key stems]
  (doseq [stem (set stems)]
    (send index update-in [stem] #(dissoc % key))))

;; ## Functions that take blocks of text

(defn index-text
  "Indexes a chunk of text.

   - `key` is an id for your reference.
   - `data` can be:
     - single string
     - a seq of [txt weight] pairs.
   - `weight` is a multiplier for occurrences.

   For example:

       (index doc-id doc-body) ;; defaults to weight=1
       (index doc-id doc-body 3)
       (index doc-id [[doc-body 1] [doc-title 2] [doc-tags 2]])
  "
  ([index key txt weight]
   (index-text index key [[txt 1]]))
  ([index key data]
   (->>
     (let [data (if (coll? data) data [[data 1]])]
       (for [[text-block multiplier] data]
         (into {}
         (->> (stems text-block)
              (frequencies)
              (map (fn [[stem num]] [stem (* num multiplier)]))))))
     (apply merge-with +)
     (map (fn [[stem num]] (add-entry index stem key num)))
     (doall))))

(defn unindex-text
  "remove all entries for [key txt]."
  [index key txt]
  (remove-entries index key (stems txt))
  nil)

(defn unindex-all
  "remove *all* entries for key. Traverses the entire index."
  [index key]
  (send index (fn [state] (reduce #(update-in %1 [%2] dissoc key) state (keys state))))
  nil)

;; ## Disk persistence

(defn save-index
  "Serialise an index to a file. The index stores the filename in metadata,
  and will save to that on subsequent saves."
  ([index] ; save to previous filename
   (if-let [{:keys [file]} (meta @index)]
     (binding [*out* (writer file)]
       (prn @index))
     (throw (Exception. "No filename specified")))
   index)
  ([index filename-or-file] ; save to specified filename
   (send index with-meta {:file filename-or-file})
   (await index)
   (save-index index)))

(defn load-index
  "Load a serialised index from a file, storing the filename in meta."
  [index filename-or-file]
  (send index (fn [_] (with-meta (with-in-reader filename-or-file (read *in*))
                                 {:file filename-or-file}))))
