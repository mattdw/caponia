(ns caponia.stemmer
  (:use clojure.contrib.str-utils))

(def *excluded-words* (with-meta #{"the" "and" "was" "are" "not" "you" "were" "that" "this" "did" "etc" "there" "they" "our" "their"}
                                {:doc "Words shorter than three characters are automatically excluded;
                                       this list only needs to contain words >=3 chars in length."}))

(declare stem)
(defn tokenise
  [#^String txt]
  (->> txt
       #^String (re-gsub #"[^-\d\w]+" " ")
       (.toLowerCase)
       (re-split #"\s+")
       ;; hyphenated-word -> [hyphenated-word hyphenated word]
       (mapcat (fn [#^String w] (if (.contains w "-") (conj (seq (.split w "-")) w) [w])))
       (filter #(and (>= (count %) 3) (not (some *excluded-words* %)) (<= (count %) 30)))
       (map stem)))

(def stem
  #^{:doc "An implementation of the Porter Stemmer algorithm, detailed at
           http://tartarus.org/~martin/PorterStemmer/"
     :tag String}
  (let [c "[^aeiou]"
        v "[aeiouy]"
        cs (str c "[^aeiouy]*")
        vs (str v "[aeiou]*")
        mgr0 (re-pattern (str "^(" cs ")?" vs cs))
        meq1 (re-pattern (str "^(" cs ")?" vs cs "(" vs ")?$"))
        mgr1 (re-pattern (str "^(" cs ")?" vs cs vs cs))
        s_v  (re-pattern (str "^(" cs ")?" vs))

        step1a-re1 #"^(.+?)(ss|i)es$"
        step1a-re2 #"^(.+?)([^s])s$"
        step1a (fn [w]
                 (cond
                   (re-find step1a-re1 w) (re-sub step1a-re1 "$1$2" w)
                   (re-find step1a-re2 w) (re-sub step1a-re2 "$1$2" w)
                   :otherwise w))

        step1b-re1 #"^(.+?)eed$"
        step1b-re2 #"^(.+?)(ed|ing)$"
        step1b-stem1 #"(at|bl|iz)$"
        step1b-stem2 #"([^aeiouylsz])\1$"
        step1b-stem3 (re-pattern (str "^" cs v "[^aeiouwxy]$"))
        step1b (fn [w]
                 (let [groups1 (re-find step1b-re1 w)
                       groups2 (re-find step1b-re2 w)]
                   (cond
                     groups1 (if (re-find mgr0 (groups1 1))
                               (apply str (butlast w))
                               w)
                     groups2 (let [stem (groups2 1)]
                               (if (re-find s_v stem)
                                 (cond
                                   (re-find step1b-stem1 stem) (str stem "e")
                                   (re-find step1b-stem2 stem) (apply str (butlast stem))
                                   (re-find step1b-stem3 stem) (str stem "e")
                                   :otherwise stem)
                                 w))
                     :otherwise w)))

        step1c-re1 #"^(.+?)y$"
        step1c (fn [w]
                 (if-let [[_ stem & _] (re-find step1c-re1 w)]
                   (if (re-find s_v stem) (str stem "i") w)
                   w))

        apply-suffix-map (fn [suffix-map #^String w]
                           (if-let [[stem suffix]
                                    (first (for [key (keys suffix-map)
                                                 :when (.endsWith w key)]
                                             [(.substring w 0 (- (count w) (count key))) key]))]
                             (if (re-find mgr0 stem)
                               (str stem (suffix-map suffix))
                               w)
                             w))

        step2-map {"ational" "ate"
                   "tional" "tion"
                   "enci" "ence"
                   "anci" "ance"
                   "izer" "ize"
                   "bli" "ble"
                   "alli" "al"
                   "entli" "ent"
                   "eli" "e"
                   "ousli" "ous"
                   "ization" "ize"
                   "ation" "ate"
                   "ator" "ate"
                   "alism" "al"
                   "iveness" "ive"
                   "fulness" "ful"
                   "ousness" "ous"
                   "aliti" "al"
                   "iviti" "ive"
                   "biliti" "ble"
                   "logi" "log"}
        step2 (partial apply-suffix-map step2-map)

        step3-map {"icate" "ic"
                   "ative" ""
                   "alize" "al"
                   "iciti" "ic"
                   "ical" "ic"
                   "ful" ""
                   "ness" ""}
        step3 (partial apply-suffix-map step3-map)

        step4-suffixes1 ["al" "ance" "ence" "er" "ic" "able" "ible" "ant" "ement"
                         "ment" "ent" "ou" "ism" "ate" "iti" "ous" "ive" "ize"]
        step4-re1 #"^(.+?)(s|t)(ion)$"
        step4 (fn [#^String w]
                (if-let [stem (first (for [suffix step4-suffixes1
                                           :when (.endsWith w suffix)]
                                       (.substring w 0 (- (count w) (count suffix)))))]
                  (if (re-find mgr1 stem) stem w)
                  (if-let [groups (re-find step4-re1 w)]
                    (let [stem (str (groups 1) (groups 2))]
                      (if (re-find mgr1 stem) stem w))
                    w)))

        step5-re1 #"^(.+?)e$"
        step5-re2 (re-pattern (str "^" cs v "[^aeiouwxy]$"))
        step5 (fn [w]
                (if-let [[_ stem & _] (re-find step5-re1 w)]
                  (if (or (re-find mgr1 stem)
                          (and (re-find meq1 stem) (not (re-find step5-re2 stem))))
                    stem
                    w)
                  w))

        step6-re #"ll$"
        step6 (fn [w]
                (if (and (re-find step6-re w) (re-find mgr1 w))
                  (apply str (butlast w))
                  w))

        step-y1 (fn [#^String w]
                  (let [firstch (.substring w 0 1)
                        firstch-y? (= firstch "y")]
                    [firstch-y? (if firstch-y?
                                  (str "Y" (.substring w 1))
                                  w)]))
        step-y2 (fn [firstch-y? #^String w]
                  (if firstch-y?
                    (str (.toLowerCase #^String (.substring w 0 1))
                         (.substring w 1))
                    w))]

    (fn [word]
      (if (< (count word) 3)
        word
        (let [[starts-with-y? w] (step-y1 word)]
          (reduce #(%2 %1) w
                  [step1a step1b step1c step2 step3 step4 step5 step6
                   (partial step-y2 starts-with-y?)]))))))
