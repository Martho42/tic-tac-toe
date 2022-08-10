(ns martho.core
  (:require [clojure.pprint :refer :all]))

(def board-indices
  (mapcat #((fn [v c] 
              (map (fn [x] (vector v x)) c))
            % [0 1 2]) 
          [0 1 2]))

(def start-board 
  (assoc
    (apply merge (map #(hash-map % 0) board-indices))
    :turn 1))

(defn print-board [board]
  (prn "The board")
  (doseq [y [0 1 2] ]
    (prn (apply str (map #(format "%3d" (board [% y])) [0 1 2]))))
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
        hseqs (map (fn [x] (filter #(= x (first %)) squares)) [0 1 2])
        vseqs (map (fn [x] (filter #(= x (second %)) squares)) [0 1 2])
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
               (prn (str "Player " (/ victor 3) " wins!"))
               (assoc board :turn 0))
      tie? (do (prn "It's a tie!")
               (assoc board :turn 0))
      true (do (prn (str "Player " (:turn board) "'s turn"))
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
        (recur (turn board [(Character/digit (first line) 10)
                            (Character/digit (last line) 10)]))))))
