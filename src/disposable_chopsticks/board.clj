(ns disposable-chopsticks.board
  (:gen-class))

(defprotocol IBoard
  (state [this])
  (actions [this turn])
  (uniq-actions [this turn])
  (next-board [this action])
  (won? [this]))

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

(defn- -sorted-hands [{first-hands :1st second-hands :2nd}]
  (concat (sort first-hands) (sort second-hands)))

(defn- -uniq-actions [board turn]
  (vals
   (reduce (fn [accr [state action]]
             (if (contains? accr state)
               accr
               (assoc accr state action)))
           {}
           (map (fn [action]
                  [(-sorted-hands (state (next-board board action))) action])
                (actions board turn)))))

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

(defn- -won? [{first-hands :1st second-hands :2nd}]
  (cond
    (= first-hands [0 0]) :2nd
    (= second-hands [0 0]) :1st))

(deftype Board [state]
  IBoard
  (state [_] state)
  (actions [this turn] (-actions (-hands this turn) turn))
  (uniq-actions [this turn] (-uniq-actions this turn))
  (next-board [_ action] (Board. (-next-board state action)))
  (won? [_] (-won? state)))

(defn new-board
  [& {first-hands :1st second-hands :2nd}]
  (->Board
   {:1st (or first-hands [1 1])
    :2nd (or second-hands [1 1])}))

