(ns martho.core-test
  (:require
   [martho.core :as core]
   [clojure.test :refer [deftest is]]))

(deftest t-turn
  (let [board core/start-board]
    (is (= {:turn -1,
            :state {[2 2] 0,
                    [0 0] 1,
                    [1 0] 0,
                    [1 1] 0,
                    [0 2] 0,
                    [2 0] 0,
                    [2 1] 0,
                    [1 2] 0,
                    [0 1] 0}}
           (core/turn board [0 0])))))

(deftest t-win
  (let [board (loop [board core/start-board
                     [move & moves] [[0 0] [0 1] [1 1] [0 2] [2 2]]]
                (if move
                  (recur (core/turn board move) moves)
                  board))]
    (is (zero? (:turn board)))))
