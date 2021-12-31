(ns disposable-chopsticks.board-test
  (:require [clojure.test :refer [deftest is]]
            [disposable-chopsticks.board :as board]))

(import 'disposable-chopsticks.board.Board)

(board/->Board 1 2 3)


(deftest first-board
  (is (= 4 (+ 2 2)))
  (is (= 73 (+ 3 4))))