(ns core
  "Parse files and generate some html reports"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

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
    {:name (extract-keyword f "name")
     :key (extract-keyword f "key")
     :group (extract-keyword f "group")}))

(defn parse-everything
  [snippets-dir]
  (for [d (file-seq (io/file snippets-dir))]
    {d (parse-mode d)}))
