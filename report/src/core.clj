(ns core
  "Parse files and generate some html reports"
  (:require [clojure.string :as str]
            [hiccup.core :as hiccup]
            [clojure.java.io :as io]))

;; TODO: do something whenever the mode has just `.yas-parents` and nothing else?
(def bulma-url "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.css")

(defn kw-pattern
  [kw]
  (re-pattern (format "# %s: (\\w+)" kw)))

(defn extract-keyword
  [filename keyword]
  (let [lines (-> filename
                  slurp
                  str/split-lines)]
    (first
     (remove nil?
             (map #(last (re-find (kw-pattern keyword) %)) lines)))))

(defn mode-files
  [mode-dir]
  (filter #(.isFile %)
          (file-seq (io/file mode-dir))))

(defn parse-mode
  [mode-dir]
  (for [f (mode-files mode-dir)]
    {:filename f
     :name (extract-keyword f "name")
     :key (extract-keyword f "key")
     :group (extract-keyword f "group")}))

(defn parse-everything
  [snippets-dir]
  (into {}
        (remove nil?)
        (for [d (file-seq (io/file snippets-dir))]
          (when (.isDirectory d)
            {(.getName d) (parse-mode d)}))))

(defn store-to-edn
  [snippets-dir output-file]
  (spit output-file (parse-everything snippets-dir)))

(def header [:name :key :group :filename])

(defn table
  "Generate a table"
  [header rows]
  [:table.table.is-striped
   [:thead (into [:tr.tr]
                 (for [h header]
                   [:td.td (name h)]))]

   (into [:tbody.tbody]
         (for [r rows]
           (into [:tr.tr]
                 (for [h header]
                   [:td.td (r h)]))))])

(defn structure
  [body]
  [:html
   [:head
    [:link {:rel "stylesheet"
            :href bulma-url
            :crossorigin "anonymous"}]]
   [:body body]])

(defn gen-html
  [snips-dir]
  (let [all-modes (parse-everything snips-dir)
        tables (for [[m snips] (take 10 (sort all-modes))]
                 [:div
                  [:h2 m]
                  (table header snips)])]
    (spit
     "hello.html"
     (hiccup/html tables))))

(comment
  (gen-html "../snippets"))
