(ns disposable-chopsticks.board-test
  (:require [clojure.test :refer [deftest is]]
            [disposable-chopsticks.board :refer :all]))

(deftest initial-board
  (is (= (internal (board))
         [[1 1] [1 1]]))
  (is (= (internal (board :first [1 2] :second [3 4]))
         [[1 2] [3 4]])))

