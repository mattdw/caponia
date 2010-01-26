(ns caponia-stemmer-test
  (:use
     clojure.test
     caponia.stemmer
     caponia-stemmer-test-cases))

(deftest stems
  (doseq [[input output] test-cases]
    (is (= (stem input) output) (str "for " input))))
