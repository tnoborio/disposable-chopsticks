(ns disposable-chopsticks.board-test
  (:require [clojure.test :refer [deftest is]]
            [disposable-chopsticks.board :refer :all]))

(deftest initial-board-test
  (is (= (state (new-board))
         {:1st [1 1] :2nd [1 1]}))
  (is (= (state (new-board :1st [1 2] :2nd [3 4]))
         {:1st [1 2] :2nd [3 4]})))

(deftest first-action-test
  (let [first-actions (actions (new-board) :1st)]
    (is (= (count first-actions) 6))
    (is (some #{[0 1 1]} first-actions))
    (is (some #{[1 0 1]} first-actions))
    (is (some #{[0 2]} first-actions))
    (is (some #{[0 3]} first-actions))
    (is (some #{[1 2]} first-actions))
    (is (some #{[1 3]} first-actions))))

(deftest second-action-test
  (let [second-actions (actions (new-board :1st [1 2] :2nd [3 0]) :2nd)]
    (is (= (count second-actions) 4))
    (is (some #{[2 3 1]} second-actions))
    (is (some #{[2 3 2]} second-actions))
    (is (some #{[2 0]} second-actions))
    (is (some #{[2 1]} second-actions))))

(deftest loop-my-action-test
  (is (not (some #{[0 1 1]} (actions (new-board :1st [2 1] :2nd [0 1]) :1st))))
  (is (not (some #{[1 0 2]} (actions (new-board :1st [1 3] :2nd [0 1]) :1st)))))

(deftest opponent-cant-be-place-action-test
  (is (not (some #{2}
                 (map first (actions (new-board :1st [2 1] :2nd [0 0]) :1st)))))
  (is (not (some #{3}
                 (map first (actions (new-board :1st [1 4] :2nd [0 0]) :1st))))))

(deftest first-uniq-action-test
  (let [first-actions (uniq-actions (new-board) :1st)]
    (is (= (count first-actions) 2))
    (is (some #{[0 1 1]} first-actions))
    (is (some #{[0 2]} first-actions))))

(deftest next-board-test
  (is (= (state (next-board (new-board) [0 1 1]))
         {:1st [0 2] :2nd [1 1]}))
  (is (= (state (next-board (new-board) [1 3]))
         {:1st [1 1] :2nd [1 2]}))
  (is (= (state (next-board (new-board :1st [4 0] :2nd [1 2]) [2 0]))
         {:1st [0 0] :2nd [1 2]}))
  (is (= (state (next-board (new-board :1st [4 0] :2nd [1 2]) [3 0]))
         {:1st [1 0] :2nd [1 2]}))
  (is (= (state (next-board (new-board :1st [0 4] :2nd [1 2]) [2 1]))
         {:1st [0 0] :2nd [1 2]})))

(deftest won?-test
  (is (= (won? (new-board)) nil))
  (is (= (won? (new-board :1st [0 0] :2nd [2 3])) :2nd))
  (is (= (won? (new-board :1st [2 3] :2nd [0 0])) :1st)))
