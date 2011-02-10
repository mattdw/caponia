A simple in-memory full-text-search engine for Clojure, named after a spider.

Getting started:

    ;; init
    (use ['caponia 'index 'query])
    (def fti (make-index))

    ;; load it up
    (index-text fti "document-1" "a blob of text")

    ;; you can provide multiple [field weight] pairs. weights are just
    ;; used as a multiplier for occurrences, for instance to rank title
    ;; matches higher than body matches.
    (index-text fti "document-1" [[document-title 2] [document-body 1]])

    ;; query it
    (do-search fti "blob") ; "and" query by default
    (do-search fti "a search phrase" :or)

    ;; save to/load from disk
    (save-index fti "directory/filename.index")
    ;; ...make some changes...
    (save-index fti) ; it remembers the filename

    (load-index fti "directory/filename.index")
    ;; ...make some changes...
    (save-index fti) ; load also stores the filename

Matt Wilson wrote this, with contributions from Andrew Brehaut. It's named after one of [these  guys](http://en.wikipedia.org/wiki/Caponiidae).
