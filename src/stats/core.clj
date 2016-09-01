(ns stats.core
  (:require [me.raynes.fs :as fs]
            [instaparse.core :as insta]))


(def reading-list-dir "/Users/clarkkampfe/code/personal/zeroclarkthirty.com/posts")

(def reading-list-files (fs/find-files reading-list-dir
                                       #".*what-im-reading\.md"))

(def rl-parser
  (insta/parser
   "S = PRELUDE ITEMS
    PRELUDE = <DASHES> <'\\n'> <LAYOUT> <'\\n'> TITLE <'\\n'> CREATED <'\\n'> <DASHES> <'\\n'+>
    DASHES = '---'
    LAYOUT = 'layout: post'
    TITLE = <'title: '> #'.+'
    CREATED = <'created: '> #'.+'
    ITEMS = ITEM+
    ITEM = LINK <'\\n'+> QUOTES
    QUOTES = QUOTE* | CODE_BLOCK*
    LINK = LINK_TITLE URL
    LINK_TITLE = <'['> #'(?U)[ \\”\\“\\’\\;\\–\\>\\{\\}\\­\\=\\!\\@\\—\\$\\#\\%\\p{Alnum}\\/\\'\\\"\\w\\d\\s\\:\\.\\?\\-\\&\\,\\(\\)]+' <']'>
    URL = <'('> #'http[\\w\\d\\s\\:\\/\\.\\-\\#\\%\\~\\?\\@\\=\\&]+' <')'>
    QUOTE = (<'- '> | <#' *\\d+\\.'>) #'.+' <'\\n'*>
    CODE_BLOCK = '{% highlight scala %}' '\\n'+ (#'[\\|\"\\*\\#\\!\\/\\w\\s\\:\\=\\\"\\.\\+\\(\\)\\%\\-\\,\\>\\$\\{\\}\\\\]+' / <'\\n'+> '{% endhighlight %}') <'\\n'*>

"
   ))

(defn t [tree] (insta/transform
                {:S (fn [prelude items] (assoc {}
                                               :prelude (->> (drop 1 prelude) (into {}))
                                               :items (drop 1 items)))

                 :ITEM #(assoc {}
                               :link %1
                               :quotes (->> %2
                                            (drop 1)
                                            (mapcat (partial drop 1))))

                 :TITLE #(->> % (assoc {} :title))

                 :LINK #(assoc {}
                               :link_title (second %1)
                               :url (second %2)
                               )

                 }
                tree))

;; analytics functions

(def parse-with-transform
  (comp t
        rl-parser))

(defn pt [n] (->> reading-list-files
                  (take (if (> n 0) n (count reading-list-files)))
                  (map slurp)
                  (map parse-with-transform)))

(def items-count
  (comp (map :items)
        (map count)))

(def quote-count-per-post
  (comp (map :items)
        (map #(mapcat (fn [x] (:quotes x))
                   %))
        (map count)))

(defn avg-quote-counts [quote-counts]
  (let [posts-count (* 1.0 (count quote-counts))]
    (/ (apply + quote-counts)
       posts-count)))

(defn median-quote-count [quote-counts]
  (nth (sort quote-counts)
       (/ (count quote-counts) 2)))

(def avg-quote-count-per-post
  (comp avg-quote-counts
        (fn [s] (sequence quote-count-per-post s))))

(def med-quote-count-per-post
  (comp median-quote-count
        (fn [s] (sequence quote-count-per-post s))))


(def quote-count-per-link
  (comp (map :items)
        (map #(map (fn [x] (:quotes x))
                      %))
        (map #(map (fn [x] (count x))
                   %))))

(def avg-quote-count-per-link
  (comp quote-count-per-link
        (map (fn [counts] (/ (* 1.0 (apply + counts))
                             (count counts))))))

(defn avg-quote-count-per-link-per-post [post]
  (zipmap (map #(:prelude %) post)
          (sequence avg-quote-count-per-link post)))
