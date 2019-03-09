(ns core
  "Parse files and generate some html reports"
  (:require [clojure.string :as str]
            [hiccup.core :as hiccup]
            [clojure.java.io :as io]))

;; TODO: do something whenever the mode has just `.yas-parents` and nothing else?
(def bulma-url "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.css")

(defn kw-pattern
  [kw]
  (re-pattern (format "# %s: (.*)" kw)))

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
    {:filename (.getName (io/file mode-dir f))
     :name (extract-keyword f "name")
     :key (or (extract-keyword f "key")
              (extract-keyword f "name"))
     :group (extract-keyword f "group")
     :desc (extract-keyword f "desc")}))

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

(def header [:name :key :filename :group :desc])

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
   (into [:body] body)])

(defn gen-html
  [snips-dir]
  (let [all-modes (parse-everything snips-dir)
        tables (for [[m snips] (sort all-modes)]
                 [:div.section
                  [:h2.subtitle m]
                  (table header snips)])]
    (spit
     "hello.html"
     (hiccup/html
      (structure tables)))))

(def mode-line "# -*- mode: snippet -*-")

(defn all-snips
  [d]
  (filter #(and (.isFile %)
                (not= (.getName %) ".yas-parents")
                (not (.endsWith (str %) ".el")))

          (file-seq (io/file d))))

(defn fix-all-modelines
  [snips]
  (doseq [s snips]
    (let [content (-> s slurp str/split-lines)
          f-line (first content)]

      (when-not (str/includes? f-line "# -*- mode")
        (println "Writing to " s)
        (spit s (str/join "\n" (cons mode-line content)))))))

(comment
  (gen-html "../snippets"))
