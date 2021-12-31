(ns disposable-chopsticks.core
  (:use [clojure.set :only [map-invert]]
        [clojure.string :only [split]])
  (:gen-class))

(defn get-winner-message
  "Returns a string message of the winner"
  [winner]
  (str "Player " (board-symbol-map winner) " has wone. Congrats!"))

(defn get-curr-player-message
  [curr-player]
  (let [base (str "Current player: " (board-symbol-map curr-player))]
    (if (= curr-player computer)
      (str base " (the computer)")
      base)))

(get-curr-player-message :1st)

(defn get-prev-player
  [n]
  (whose-turn (mod (- n 1) 2)))

(defn get-other-player
  [curr-player]
  (cond
    (= curr-player :x) :o
    (= curr-player :o) :x))

;;
;; Min-max and related functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn utility
  "Utility function for the min-max algorithm.
  :x is the max player and :o is the min player" ; Computer is :o
  [board]
  (cond
    (has-player-won board :x) 1
    (has-player-won board :o) -1
    :else nil))

(defn get-nil-indices
  "For a particular row, returns the indices of its nil items."
  [row]
  (keep-indexed #(if (nil? %2) %1) row))

(defn attach-row-coord
  "Given a row-coord (e.g. 2) and an items list e.g. (0 1), returns a list of lists where
  row-coord is attached to each item e.g. ( (2 0) (2 1) )"
  [row-coord items-list]
  (map #(list row-coord %) items-list))

(defn get-empty-squares
  "returns a list of (row, col) lists that are the coords of the nil squares in board"
  [board]
  (apply concat (keep-indexed #(attach-row-coord %1 %2)
                              (map get-nil-indices board))))

(defn get-all-actions
  "Returns a list of all possible actions (boards) for the curr-player from board"
  [board curr-player]
  (map (fn [x] (update-board board (first x) (last x) curr-player)) (get-empty-squares board)))


(defn get-comparison-fn
  "Returns the appropriate comparison function for minmax"
  [curr-player]
  (cond
    (= curr-player :x) >
    (= curr-player :o) <))

(defn minmax
  "Recursive minmax implementation.
  n is the relative depth of board, i.e. 1 = 1 ply search, 2 = 2 ply search, etc..."
  [curr-board curr-player counter]
  (let [cur-util (utility curr-board)]
    (if (not (nil? cur-util))            ; stopping criteria in recursive call
      cur-util
      (do
        (let [actions (get-all-actions curr-board curr-player)
              comp-fn (get-comparison-fn curr-player)]
          (let [action-util-pairs (map (fn [a] (list a (minmax a (get-other-player curr-player) (inc counter)))) actions) ; recursively create tree
                best-one (first (sort-by last comp-fn action-util-pairs))]        ; percolate best-move up the tree
;(println (str "DEBUG: best one is: " best-one " counter is: " counter))
            (if (== counter 1)
              (first best-one)            ; choose best move by board
              (last best-one))))))))      ; choose best move by utility function

(defn apply-move
  [board curr-player]
  (if (= curr-player computer)
    (do
      (minmax board curr-player 1))
    (do
      (println "Your move: (<from letter><to letter><number>)")
      (let [action (get-action (read-line))]
        (println)
        (if (nil? action)
          board
          (update-board board action curr-player))))))

;;
;; Main routine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main
  [& args]
  (println  "Starting waribashi game.")
  (loop [n 0
         board (get-empty-board)]
    (let [curr-player (whose-turn (mod n 2))
          prev-player (get-prev-player n)]
      (println (get-curr-player-message curr-player))
      (print-board board)
      (cond
        (has-player-won board prev-player)
        (println (get-winner-message prev-player))
        :else
        (do
          (let [new-board (apply-move board curr-player)]
            (cond
              (= new-board board)
              (do
                (println "Invalid move. Try again\n")
             ; There was an invalid move, so keep the same player
                (recur n board))
              :else ; valid move, so move on to next player
              (recur (+ n 1) new-board))))))))