(ns disposable-chopsticks.board
  (:use [clojure.set :only [map-invert]]
        [clojure.string :only [split]])
  (:gen-class))

(defprotocol IBoard
  (state [this])
  (actions [this turn])
  (next-board [this action]))

(defn- -hands [board turn]
  (let [{first-hands :1st second-hands :2nd} (state board)]
    (if (= turn :1st)
      [first-hands second-hands]
      [second-hands first-hands])))

(defn- -my-actions [[left right]]
  (concat
   (for [i (range 1 (inc left))
         :when (not= [left right] [(+ right i) (- left i)])]
     [0 1 i])
   (for [i (range 1 (inc right))
         :when (not= [left right] [(- right i) (+ left i)])]
     [1 0 i])))

(defn- -opposite-actions [[left right] inc-index opposite-hand opposite-index]
  (filter identity
          [(when (and (> left 0)
                      (> opposite-hand 0))
             [(+ 0 inc-index) opposite-index])
           (when (and (> right 0)
                      (> opposite-hand 0))
             [(+ 1 inc-index) opposite-index])]))

(defn- -actions [[my-hands [opposite-left opposite-right]] turn]
  (let [inc-index (if (= turn :1st) 0 2)]
    (concat
     (map (fn [[from to amount]]
            (remove nil? [(+ from inc-index) (+ to inc-index) amount]))
          (-my-actions my-hands))
     (-opposite-actions my-hands inc-index opposite-left (mod (+ inc-index 2) 4))
     (-opposite-actions my-hands inc-index opposite-right (mod (+ inc-index 3) 4)))))

(defn- -next-board [state [from to amount]]
  (let [{first-hands :1st second-hands :2nd} state
        seq-hands (vec (concat first-hands second-hands))
        [first-hands second-hands]
        (->> (if amount
               (-> seq-hands
                   (assoc from (- (nth seq-hands from) amount))
                   (assoc to (+ (nth seq-hands to) amount)))
               (assoc seq-hands to (+ (nth seq-hands to) (nth seq-hands from))))
             (map #(mod %1 5))
             (partition 2))]
    {:1st first-hands :2nd second-hands}))

(deftype Board [state]
  IBoard
  (state [_] state)
  (actions [this turn] (-actions (-hands this turn) turn))
  (next-board [_ action] (Board. (-next-board state action))))

(defn board
  [& {first-hands :1st second-hands :2nd}]
  (->Board
   {:1st (or first-hands [1 1])
    :2nd (or second-hands [1 1])}))

;; (def board-symbol-map {:1st "1st" :2nd "2nd"})

;; (def whose-turn
;;   {0 :1st 1 :2nd})

;; (def player-index (map-invert whose-turn))

;; (def opponent-player {:1st :2nd, :2nd :1st})

;; (def letter-vec (seq (char-array "abcd")))

;; (defn letter-index [v]
;;   (let [index (.indexOf letter-vec v)]
;;     (when (>= index 0) index)))

;; (def computer :1st)

;; (defn print-board
;;   [board]
;;   (dotimes [n (count board)]
;;     (println "    " (apply str (interpose " " (nth (partition 2 letter-vec) n))))
;;     (println (name (whose-turn n)) (nth board n))))

;; (defn parse-int [number]
;;   (try
;;     (let [num (Integer/parseInt (str number))]
;;       (when (<= 1 num 4)
;;         num))
;;     (catch Exception e
;;       nil)))

;; (defn get-action [[from to & [amount]]]
;;   (print from to amount)
;;   (let [from (letter-index from)
;;         to (letter-index to)
;;         amount (parse-int amount)]
;;     (when (and from to)
;;       [from to amount])))

;; (get-action "ab3")

;; (defn get-modified-row
;;   [board row col curr-player]
;;   (let [working-row (nth board row)]
;;     (if (not (nth working-row col))
;;       (into [] (assoc working-row col curr-player))
;;       working-row)))

;; (defn update-board [board [from to amount] curr-player]
;;   (let [my-hand (nth board (player-index curr-player))
;;         opposite-hand (nth board (player-index (opponent-player curr-player)))
;;         attack? (or (and (#{0 1} from) (#{2 3} to))
;;                     (and (#{2 3} from) (#{0 1} to)))]
;;     (print attack?)
;;     (when (> (nth my-hand from) 0)
;;       (if attack?
;;         (if ())))))

;; (update-board [[1 2] [0 1]] [0 3] :2nd)

;; (defn has-player-won
;;   [board player]
;;   (= (nth board (player-index (opponent-player :1st))) [0 0]))

;; (defn get-winner-message
;;   "Returns a string message of the winner"
;;   [winner]
;;   (str "Player " (board-symbol-map winner) " has wone. Congrats!"))

;; (defn get-curr-player-message
;;   [curr-player]
;;   (let [base (str "Current player: " (board-symbol-map curr-player))]
;;     (if (= curr-player computer)
;;       (str base " (the computer)")
;;       base)))

;; (get-curr-player-message :1st)

;; (defn get-prev-player
;;   [n]
;;   (whose-turn (mod (- n 1) 2)))

;; (defn get-other-player
;;   [curr-player]
;;   (cond
;;     (= curr-player :x) :o
;;     (= curr-player :o) :x))

;; ;;
;; ;; Min-max and related functions
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defn utility
;;   "Utility function for the min-max algorithm.
;;   :x is the max player and :o is the min player" ; Computer is :o
;;   [board]
;;   (cond
;;     (has-player-won board :x) 1
;;     (has-player-won board :o) -1
;;     :else nil))

;; (defn get-nil-indices
;;   "For a particular row, returns the indices of its nil items."
;;   [row]
;;   (keep-indexed #(if (nil? %2) %1) row))

;; (defn attach-row-coord
;;   "Given a row-coord (e.g. 2) and an items list e.g. (0 1), returns a list of lists where
;;   row-coord is attached to each item e.g. ( (2 0) (2 1) )"
;;   [row-coord items-list]
;;   (map #(list row-coord %) items-list))

;; (defn get-empty-squares
;;   "returns a list of (row, col) lists that are the coords of the nil squares in board"
;;   [board]
;;   (apply concat (keep-indexed #(attach-row-coord %1 %2)
;;                               (map get-nil-indices board))))

;; (defn get-all-actions
;;   "Returns a list of all possible actions (boards) for the curr-player from board"
;;   [board curr-player]
;;   (map (fn [x] (update-board board (first x) (last x) curr-player)) (get-empty-squares board)))


;; (defn get-comparison-fn
;;   "Returns the appropriate comparison function for minmax"
;;   [curr-player]
;;   (cond
;;     (= curr-player :x) >
;;     (= curr-player :o) <))

;; (defn minmax
;;   "Recursive minmax implementation.
;;   n is the relative depth of board, i.e. 1 = 1 ply search, 2 = 2 ply search, etc..."
;;   [curr-board curr-player counter]
;;   (let [cur-util (utility curr-board)]
;;     (if (not (nil? cur-util))            ; stopping criteria in recursive call
;;       cur-util
;;       (do
;;         (let [actions (get-all-actions curr-board curr-player)
;;               comp-fn (get-comparison-fn curr-player)]
;;           (let [action-util-pairs (map (fn [a] (list a (minmax a (get-other-player curr-player) (inc counter)))) actions) ; recursively create tree
;;                 best-one (first (sort-by last comp-fn action-util-pairs))]        ; percolate best-move up the tree
;; ;(println (str "DEBUG: best one is: " best-one " counter is: " counter))
;;             (if (== counter 1)
;;               (first best-one)            ; choose best move by board
;;               (last best-one))))))))      ; choose best move by utility function

;; (defn apply-move
;;   [board curr-player]
;;   (if (= curr-player computer)
;;     (do
;;       (minmax board curr-player 1))
;;     (do
;;       (println "Your move: (<from letter><to letter><number>)")
;;       (let [action (get-action (read-line))]
;;         (println)
;;         (if (nil? action)
;;           board
;;           (update-board board action curr-player))))))

;; ;;
;; ;; Main routine
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defn -main
;;   [& args]
;;   (println  "Starting waribashi game.")
;;   (loop [n 0
;;          board (get-empty-board)]
;;     (let [curr-player (whose-turn (mod n 2))
;;           prev-player (get-prev-player n)]
;;       (println (get-curr-player-message curr-player))
;;       (print-board board)
;;       (cond
;;         (has-player-won board prev-player)
;;         (println (get-winner-message prev-player))
;;         :else
;;         (do
;;           (let [new-board (apply-move board curr-player)]
;;             (cond
;;               (= new-board board)
;;               (do
;;                 (println "Invalid move. Try again\n")
;;              ; There was an invalid move, so keep the same player
;;                 (recur n board))
;;               :else ; valid move, so move on to next player
;;               (recur (+ n 1) new-board))))))))
