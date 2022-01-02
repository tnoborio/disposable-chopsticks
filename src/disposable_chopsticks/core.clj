(ns disposable-chopsticks.core
  (:require [disposable-chopsticks.board :refer :all])
  (:gen-class))

(def next-player {:1st :2nd, :2nd :1st})

(def letter-vec (seq (char-array "abcd")))

(def whose-turn {0 :1st, 1 :2nd})

(def computer :2nd)

(defn print-board [board player]
  (let [{first :1st second :2nd} (state board)]
    (println (str "Current player: " (name player))
             (if (= player computer) " (the computer)" ""))
    (dotimes [n 2]
      (println "    "
               (apply str (interpose " " (nth (partition 2 letter-vec) n))))
      (println (name (whose-turn n))
               (if (= n 0) first second)))))

(defn parse-action [str]
  (map {\a 0 \b 1 \c 2 \d 3
        \1 1 \2 2 \3 3 \4 4} str))

(defn utility [board]
  (condp = (won? board)
    :1st -1
    :2nd 1
    (let [{first-hands :1st second-hands :2nd} (state board)
          first-hands (sort first-hands)
          second-hands (sort second-hands)]
      (cond
        (= first-hands [0 1]) 0.5
        (= second-hands [0 1]) -0.5

        (and (= first-hands [1 2]) (= second-hands [2 3])) -0.75
        (and (= first-hands [2 3]) (= second-hands [1 2])) 0.75

        (and (= first-hands [2 2]) (= second-hands [1 2])) -0.5
        (and (= first-hands [1 2]) (= second-hands [2 2])) 0.5
        :else nil))))

(defn comparison-fn [player]
  (if (= player computer) > <))

(defn minmax [board player counter]
  (let [cur-util (utility board)]
    ;; (prn :minmax :cur-util cur-util)
    (if (or (not (nil? cur-util)) (> counter 8))
      (or cur-util 0)
      (let [actions (uniq-actions board player)
            comp-fn (comparison-fn player)
            action-util-pairs
            (map (fn [action]
                   (list action (minmax (next-board board action) (next-player player) (inc counter)))) actions) ; recursively create tree
            best-one (first (sort-by last comp-fn action-util-pairs))]
        (if (= counter 1)
          (do
            (println :action-util-pairs action-util-pairs)
            (println :best-one best-one)
            (println :counter counter)
            (prn :best-one best-one :actions action-util-pairs)
            (next-board board (first best-one)))
          (last best-one))))))

(defn apply-move-board
  [board player]
  (if (= player computer)
    (minmax board player 1)
    (do
      (println "Your move: (<from letter><to letter><number>)")
      (let [action (parse-action (read-line))]
        (println)
        (if (or (nil? action)
                (not (some #(= action %) (actions board player))))
          board
          (next-board board action))))))

(defn -main
  [& _]
  (loop [player :1st
         board (new-board)]
    (print-board board player)
    (if-let [won-player (won? board)]
      (println (str "Player " (name won-player) " has wone. Congrats!"))
      (let [applied-board (apply-move-board board player)]
        (if (= (state applied-board) (state board))
          (do
            (println "Invalid move. Try again\n")
            (recur player board))
          (recur (next-player player) applied-board))))))

(won? (new-board :1st [0 0] :2nd [2 3]))