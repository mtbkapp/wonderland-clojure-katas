(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))


(deftest test-card-compare
  (let [sorted-cards (sort card-compare (shuffle cards))]
    (is (= (map first sorted-cards)
           (take 52 (cycle suits))))
    (is (= (map second sorted-cards)
           (take 52 (flatten (map (partial repeat 4) 
                                  (cycle ranks))))))))

;; fill in  tests for your game
(deftest test-play-round

  )

(deftest test-play-game
  (testing "the player loses when they run out of cards"))

