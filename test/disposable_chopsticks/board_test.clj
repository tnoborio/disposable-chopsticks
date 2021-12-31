(ns disposable-chopsticks.board-test
  (:require [clojure.test :refer [deftest is]]
            [disposable-chopsticks.board :refer :all]))

(deftest initial-board-test
  (is (= (state (board))
         {:1st [1 1] :2nd [1 1]}))
  (is (= (state (board :1st [1 2] :2nd [3 4]))
         {:1st [1 2] :2nd [3 4]})))

(deftest first-action-test
  (let [first-actions (actions (board) :1st)]
    (is (= (count first-actions) 6))
    (is (some #{[0 1 1]} first-actions))
    (is (some #{[1 0 1]} first-actions))
    (is (some #{[0 2]} first-actions))
    (is (some #{[0 3]} first-actions))
    (is (some #{[1 2]} first-actions))
    (is (some #{[1 3]} first-actions))))

(deftest loop-my-action-test
  (is (not (some #{[0 1 1]} (actions (board :1st [2 1] :2nd [0 1]) :1st))))
  (is (not (some #{[1 0 2]} (actions (board :1st [1 3] :2nd [0 1]) :1st)))))

(deftest opponent-cant-be-place-action-test
  (is (not (some #{2}
                 (map first (actions
                             (board :1st [2 1] :2nd [0 0]) :1st)))))
  (is (not (some #{3}
                 (map first (actions
                             (board :1st [1 4] :2nd [0 0]) :1st))))))

