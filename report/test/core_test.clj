(ns core-test
  (:require [clojure.test :refer [deftest testing is]]
            [core :as sut]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(deftest unique-test
  (let [all-modes (sut/parse-everything "../snippets")]
    (doseq [[k snippets] all-modes]
      (let [keys (map :name snippets)
            freq (->> (frequencies keys)
                      (filter #(> (second %) 1)))]
        (is (= (count keys) (count (set keys)))
            (str "failed for mode " k " and failing things:\n"
                 (str/join ", " (map first freq))))))))

(defn with-nil-key [ns key]
  (str/join ", " (map :filename (filter #(nil? (key %)) ns))))

(deftest non-nils-test
  (let [all-modes (sut/parse-everything "../snippets")]
    (doseq [[k snippets] all-modes]
      (let [ns (filter #(not (.startsWith (:filename %) ".yas-")) snippets)
            nil-names (with-nil-key ns :name)
            nil-keys (with-nil-key ns :key)]
        (is (empty? nil-names) (str "nils in " k " => " nil-names))
        (is (empty? nil-keys) (str "nils in " k "=> " nil-keys))))))

(comment
  (def reg #"key:(\w)")

  (doseq [f (file-seq (io/file "../snippets"))
          :when (.isFile f)
          :let [c (slurp f)]]

    (when (re-seq reg c)
      (let [new-c (str/replace c reg "key: $1")]
        (spit (.getPath f) new-c))))

  (doseq [f (file-seq (io/file "../snippets"))
          :when (.isFile f)
          :let [c (slurp f)
                res (re-seq #"contributor:" c)]]

    ;; should I clean all of them?
    (when (seq res)
      (println "file = " f))))
