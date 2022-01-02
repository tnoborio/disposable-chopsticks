(ns disposable-chopsticks.core
  (:require [disposable-chopsticks.board :refer :all])
  (:gen-class))

(def next-player {:1st :2nd, :2nd :1st})

(defn print-board [board player computer]
  (let [{first :1st second :2nd} (state board)]
    (println (str "Current player: " (name player))
             (if (= player computer) " (the computer)" ""))
    (dotimes [n 2]
      (println "    "
               (apply str (interpose " " (nth ["ab" "cd"] n))))
      (println (name ({0 :1st, 1 :2nd} n))
               (if (= n 0) first second)))))

(defn parse-action [str]
  (map {\a 0 \b 1 \c 2 \d 3
        \1 1 \2 2 \3 3 \4 4} str))

(defn utility [board turn]
  (condp = (won? board)
    :1st (if (= turn :2nd) 1 -1)
    :2nd (if (= turn :1st) 1 -1)
    (let [{first-hands :1st second-hands :2nd} (state board)
          [first-hands second-hands] [(sort first-hands) (sort second-hands)]
          my-hands (if (= turn :1st) first-hands second-hands)
          op-hands (if (= turn :2nd) second-hands first-hands)]
      (cond
        (= my-hands [0 1]) 0.5
        (= op-hands [0 1]) -0.5

        (and (= my-hands [1 2]) (= op-hands [2 3])) -0.75
        (and (= my-hands [2 3]) (= op-hands [1 2])) 0.75

        (and (= my-hands [2 2]) (= op-hands [1 2])) -0.5
        (and (= my-hands [1 2]) (= op-hands [2 2])) 0.5

        :else nil))))

(defn minmax [board player computer counter]
  (let [cur-util (utility board player)]
    (if (or (not (nil? cur-util)) (> counter 8))
      (or cur-util 0)
      (let [actions (uniq-actions board player)
            comp-fn (if (= player computer) > <)
            action-pairs
            (map #(list % (minmax (next-board board %) (next-player player) computer (inc counter))) actions)
            best-one (first (sort-by last comp-fn action-pairs))]
        (if (= counter 1)
          (do
            (println :action-pairs action-pairs :best-one best-one)
            (next-board board (first best-one)))
          (last best-one))))))

(defn apply-move-board [board player computer]
  (if (= player computer)
    (minmax board player computer 1)
    (do
      (println "Your move: (<from letter><to letter><number>)")
      (let [action (parse-action (read-line))]
        (println)
        (if (or (nil? action)
                (not (some #(= action %) (actions board player))))
          board
          (next-board board action))))))


(defn -main [{:keys [computer board] :or {computer :2nd board [[1 1] [1 1]]}}]
  (loop [player :1st
         board (new-board :1st (first board) :2nd (second board))]
    (print-board board player computer)
    (if-let [won-player (won? board)]
      (println (str "Player " (name won-player) " has wone. Congrats!"))
      (let [applied-board (apply-move-board board player computer)]
        (if (= (state applied-board) (state board))
          (do
            (println "Invalid move. Try again\n")
            (recur player board))
          (recur (next-player player) applied-board))))))

