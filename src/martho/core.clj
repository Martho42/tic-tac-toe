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
  (assoc
    (apply merge (map #(hash-map % 0) board-indices))
    :turn 1))

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
    (prn (apply str y ":" (map #(str " " (get-marker (board [% y]))) (range 3)))))
  (prn)
  board)

(defn mark [board square]
  (if (= (board square) 0)
    (-> board 
        (assoc  square (:turn board))
        (update :turn * -1))
    (do (prn "Invalid Square")
        board)))

(def win-sequences 
  (let [squares (filter vector? (keys start-board))
        hseqs (map (fn [x] (filter #(= x (first %)) squares)) (range 3))
        vseqs (map (fn [x] (filter #(= x (second %)) squares)) (range 3))
        cross1 [(filter #(= (first %) (second %)) squares)]
        cross2 [(filter #(= 2 (+ (first %) (second %))) squares)]]
    (concat hseqs vseqs cross1 cross2)))



(defn check-end [board]
  (let [victor (first (filter #(= 3 (abs %))
                              (map #(apply + %)
                                   (map (fn [x]
                                          (map #(board %) x))
                                        win-sequences))))

        tie? (= 8 (count (filter #(and (some #{1} %) (some #{-1} %))
                              (map (fn [x]
                                          (map #(board %) x))
                                        win-sequences))))]
    (cond
      victor (do 
               (prn (str "Player " (get-marker (/ victor 3)) " wins!"))
               (assoc board :turn 0))
      tie? (do (prn "It's a tie!")
               (assoc board :turn 0))
      true (do (prn (str "Player " (get-marker (:turn board)) "'s turn. Enter a letter followed by a number:"))
              board))))

(defn turn [board square]
  (-> board
      (mark square)
      (print-board)
      (check-end)))

(defn -main []
  (print-board start-board)
  (check-end start-board)
  (loop [board start-board]
    (when (not= 0 (:turn board))
      (flush)
      (let [line (read-line)]
        (recur (turn board [(- (int (first line)) 97) 
                            (Character/digit (last line) 10)]))))))
