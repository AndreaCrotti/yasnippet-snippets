(ns core
  (:require [clojure.string :as str]))

(defn extract-name
  [filename]
  (let [lines (-> filename
                  slurp
                  str/split-lines)]
    (remove nil?
            (map #(last (re-find #"# name: (\w+)" %)) lines))))
