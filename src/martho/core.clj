(ns martho.core
  (:require [clojure.pprint :as ppr]
            [clojure.string :as str]))

(def board-indices
  (mapcat #((fn [v c]
              (map (fn [x] (vector v x)) c))
            % (range 3))
          (range 3)))

(identity board-indices)

(def start-board
  {:turn 1
   :state (apply merge (map #(hash-map % 0) board-indices))})

(identity start-board)

(defn get-marker [p]
  (cond
    (= 0 p) "-"
    (= 1 p) "X"
    (= -1 p) "O"))

(defn print-board [board]
  (prn "The board")
  (prn "   a b c")
  (prn "   -----")
  (doseq [y (range 3)]
    (prn (apply str y ":" (map #(str " " (get-marker (get-in board [:state [% y]]))) (range 3)))))
  (prn)
  (flush)
  board)

(defn mark [board square]
  (if (= (get-in board [:state square]) 0)
    (-> board
        (assoc-in [:state square] (:turn board))
        (update :turn * -1))
    (do (prn "Invalid Square")
        board)))

(defn get-squares-keys [board]
  (filter vector? (keys start-board)))

(def win-sequences
  (let [squares (keys (:state start-board))
        hseqs (map (fn [x] (filter #(= x (first %)) squares)) (range 3))
        vseqs (map (fn [x] (filter #(= x (second %)) squares)) (range 3))
        cross1 [(filter #(= (first %) (second %)) squares)]
        cross2 [(filter #(= 2 (+ (first %) (second %))) squares)]]
    (concat hseqs vseqs cross1 cross2)))

(defn win-sequence-values
  [s board]
  (map (:state board) s))

(defn score-win-sequence
  [s board]
  (apply + (win-sequence-values s board)))

(defn win?
  [score]
  (= 3 (abs score)))

(defn tie?
  [s]
  (and (some #{1} s)
       (some #{-1} s)))

(defn check-end [board]
  (let [victor (->> win-sequences
                    (map #(score-win-sequence % board))
                    (filter win?)
                    first)
        tied-game? (= 8 (->> win-sequences
                             (map #(win-sequence-values % board))
                             (filter tie?)
                             count))]
    (cond
      victor (do
               (prn (str "Player " (get-marker (/ victor 3)) " wins!"))
               (assoc board :turn 0))
      tied-game? (do (prn "It's a tie!")
                     (assoc board :turn 0))
      true (do (prn (str "Player " (get-marker (:turn board)) "'s turn. Enter a letter followed by a number:"))
               board))))

(defn turn [board square]
  (-> board
      (mark square)
      (print-board)
      (check-end)))

(defn score-win-seq [win-seq]

  )

(defn select-move [board]
  (key (rand-nth (filter #(zero? (val %)) (:state board)))))

(defn -main []
  (print-board start-board)
  (check-end start-board)
  (loop [board start-board]
    (cond
      (= 1 (:turn board))
      (recur (turn board (select-move board)))
      (= -1 (:turn board)) (let [line (read-line)]
                             (recur (turn board [(- (int (first line)) 97)
                                                 (Character/digit (last line) 10)]))))))
