(ns disposable-chopsticks.core
  (:require [disposable-chopsticks.board :refer :all])
  (:gen-class))

(def next-player {:1st :2nd, :2nd :1st})

(def letter-vec (seq (char-array "abcd")))

(def whose-turn {0 :1st, 1 :2nd})

(defn letter-index [v]
  (let [index (.indexOf letter-vec v)]
    (when (>= index 0) index)))

(def computer :2nd)

(defn print-board [board]
  (let [{first :1st second :2nd} (state board)]
    (dotimes [n 2]
      (println "    "
               (apply str (interpose " " (nth (partition 2 letter-vec) n))))
      (println (name (whose-turn n))
               (if (= n 0) first second)))))

(defn parse-action [str]
  (map {\a 0 \b 1 \c 2 \d 3
        \1 1 \2 2 \3 3 \4 4} str))

(parse-action "ab3")

(defn get-winner-message
  [winner]
  (str "Player " (name winner) " has wone. Congrats!"))

(defn get-curr-player-message
  [curr-player]
  (let [base (str "Current player: " (name curr-player))]
    (if (= curr-player computer)
      (str base " (the computer)")
      base)))

(get-curr-player-message :1st)

(defn get-prev-player
  [n]
  (whose-turn (mod (- n 1) 2)))

(defn utility
  [board]
  (prn :utility board (state board))
  (condp = (won? board)
    :1st 1
    :2nd -1
    nil))

(defn attach-row-coord
  "Given a row-coord (e.g. 2) and an items list e.g. (0 1), returns a list of lists where
  row-coord is attached to each item e.g. ( (2 0) (2 1) )"
  [row-coord items-list]
  (map #(list row-coord %) items-list))

(defn get-comparison-fn
  "Returns the appropriate comparison function for minmax"
  [curr-player]
  (cond
    (= curr-player :x) >
    (= curr-player :o) <))

(defn minmax [board player counter]
  (prn :minmax board player counter)
  (let [cur-util (utility board)]
    (if (not (nil? cur-util))
      cur-util
      (let [actions (actions board player)
            comp-fn (get-comparison-fn player)]
        (println :actions actions)
        (let [action-util-pairs
              (map (fn [action] (list action (minmax (next-board board action) (next-player player) (inc counter)))) actions) ; recursively create tree
              best-one (first (sort-by last comp-fn action-util-pairs))]        ; percolate best-move up the tree
          (println :action-util-pairs action-util-pairs )
          (println :best-one best-one )
          (println (str "DEBUG: best one is: " best-one " counter is: " counter))
          (if (= counter 1)
            (first best-one)            ; choose best move by board
            (last best-one)))))))      ; choose best move by utility function

(defn apply-move-board
  [board player]
  (prn :apply-move-board board player)
  (if (= player computer)
    (do
      (minmax board player 1))
    (do
      (println "Your move: (<from letter><to letter><number>)")
      (let [action (parse-action (read-line))]
        (println)
        (if (nil? action)
          board
          (next-board board action))))))

(defn -main
  [& args]
  (loop [player :1st
         board (new-board)]
    (println (get-curr-player-message player))
    (print-board board)
    (prn "hoge")
    (if-let [won-player (won? board)]
      (println (get-winner-message won-player))
      (let [applied-board (apply-move-board board player)]
        (prn :applied-board applied-board :board board)
        (if (= (state applied-board) (state board))
          (do
            (println "Invalid move. Try again\n")
            (recur player board))
          (recur (next-player player) applied-board))))))
