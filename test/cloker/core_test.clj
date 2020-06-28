(ns cloker.core-test
  (:require [clojure.test :refer :all]
            [cloker.core :refer :all]))

(deftest c-function
  (testing "Creating a card from a string"
    (testing "succeeds with valid input"
      (doseq [rank ["2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K" "A"]
              suit ["C" "D" "H" "S"]]
        (is (not (nil? (c (str rank suit)))))))
    (testing "fails with invalid input"
      (doseq [rank ["C" "0" "1" "11"]
              suit ["A" "X" "?"]]
        (is (nil? (c (str rank suit))))))
    (testing "throws exception if preconditions are not met"
      (doseq [value ["" "A" "S" "ASAS" :not-a-string 1 {:rank "A" :suit "S"}]]
        (is (thrown? AssertionError (c value)))))))
