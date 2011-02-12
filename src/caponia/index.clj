(ns caponia.index
  (:use
   [stemmers.core :only [stems]])
  (:require
   [clojure.contrib.duck-streams :as ducks]
   [stemmers.porter]))

(defrecord Index [data stemmer])

(defn make-index
  "Create a new (empty) index. Takes an optional stemmer function"
  ([]
     (make-index stemmers.porter/stem))
  ([stemmer-func]
     (agent (Index. {} stemmer-func))))

;; ## Functions that take stems

(defn add-entry
  "Insert an entry into the index, associating {key weight} with stem"
  [index stem key weight]
  (send index assoc-in [:data stem key] weight))

(defn remove-entries
  "dissociate key from stems"
  [index key stems]
  (doseq [stem (set stems)]
    (send index update-in [:data stem] #(dissoc % key))))

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
                (->> (stems text-block (:stemmer @index))
                     (frequencies)
                     (map (fn [[stem num]] [stem (* num multiplier)]))))))
      (apply merge-with +)
      (map (fn [[stem num]] (add-entry index stem key num)))
      (doall))))

(defn unindex-text
  "remove all entries for [key txt]."
  [index key txt]
  (remove-entries index key (stems txt (:stemmer @index)))
  nil)

(defn unindex-all
  "remove *all* entries for key. Traverses the entire index."
  [index key]
  (send index update-in [:data] (fn [state] (reduce #(update-in %1 [%2] dissoc key) state (keys state))))
  nil)

;; ## Disk persistence

(defn save-index
  "Serialise an index to a file. The index stores the filename with the index,
  and will save to that on subsequent saves."
  ([index] ; save to previous filename
     (if-let [file (:file-storage-name @index)]
       (binding [*out* (ducks/writer file)]
         (prn (:data @index)))
       (throw (Exception. "No filename specified")))
     index)
  ([index filename-or-file] ; save to specified filename
     (send index assoc :file-storage-name filename-or-file)
     (await index)
     (save-index index)))

(defn load-index
  "Load a serialised index from a file, storing the filename in meta."
  [index filename-or-file]
  (send index (fn [_] {:data     (ducks/with-in-reader filename-or-file (read *in*))
                      :file-storage-name filename-or-file})))
