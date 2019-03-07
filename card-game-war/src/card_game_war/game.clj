(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn vector->rank-map
  [v]
  (zipmap v (range 0 (count v))))

(def suits-rank-map (vector->rank-map suits))
(def ranks-rank-map (vector->rank-map ranks))

(defn card-compare
  [[suit-x rank-x :as card-x] [suit-y rank-y :as card-y]]
  (let [r (compare (get ranks-rank-map rank-x)
                   (get ranks-rank-map rank-y))]
    (if-not (zero? r) 
      r
      (compare (get suits-rank-map suit-x)
               (get suits-rank-map suit-y)))))

(defn play-round 
  [[[p1-card & player1-cards] [p2-card % player2-cards]]]
  (let [c (card-compare p1-card p2-card)]
    (cond (neg? c)
          [player1-cards (conj player2-cards p1-card p2-card)]
          (pos? c)
          [(conj player1-cards p1-card p2-card) player2-cards]
          :else
          [(conj player1-cards p1-card) (conj player2-cards p2-card)])))

(defn play-game
  [game]
  
  )

(defn game
  [] 
  (let [cards (shuffle cards)]
    (->> cards 
         (partition (/ (count cards) 2))
         (map vec)
         (into []))))


