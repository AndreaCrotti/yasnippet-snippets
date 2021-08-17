(ns core-test
  (:require [clojure.test :refer [deftest testing is]]
            [core :as sut]))

(deftest unique-test
  (let [all-modes (sut/parse-everything "../snippets")]
    (doseq [[k snippets] all-modes]
      (let [keys (map :name snippets)]
        (is (= (count keys) (count (set keys))) (str "failed for mode " k))))))
