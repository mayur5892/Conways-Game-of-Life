(ns conways-game-of-life.core-test
  (:require [clojure.test :refer :all]
            [conways-game-of-life.core :refer :all]))



(deftest find-new-state-test
  (testing "this test for verifying cell new state"
    (is (seq #{[0 0] [1 0] [1 1]}) (find-next-gen-state #{[0 0] [1 0] [1 1]}))))

