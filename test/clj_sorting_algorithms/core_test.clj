(ns clj-sorting-algorithms.core-test
  (:require [clojure.test :refer :all]
            [clj-sorting-algorithms.core :refer :all]))

(def to-sort '(9 3 1 41 3 100 -5 3 -10 3))
(def sorted '(-10 -5 1 3 3 3 3 9 41 100))

(deftest sorting-algorithms
  (testing "mergesort"
    (is (= (mergesort to-sort) sorted)))
  (testing "bubblesort"
    (is (= (bubblesort to-sort) sorted)))
  (testing "insertionsort"
    (is (= (insertionsort to-sort) sorted))))
