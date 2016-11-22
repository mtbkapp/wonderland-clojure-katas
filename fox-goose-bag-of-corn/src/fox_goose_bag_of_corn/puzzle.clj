(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as sets])
  (:import [clojure.lang PersistentQueue]))

(defn where-are-you
  "Returns the key to the set that contains :you."
  [pos]
  (cond (:you (:near pos)) :near
        (:you (:boat pos)) :boat
        (:you (:far pos)) :far))

(def adjacent-sets
  {:near #{:boat}
   :far #{:boat}
   :boat #{:near :far}})

(defn move-thing
  "Moves the `thing` from the set named from `from` to the set named by `to`"
  [pos thing from to]
  (-> pos
      (update-in [from] #(disj % thing))
      (update-in [to] #(conj % thing))))

(defn items-with-you
  "Retunrs the items that :you can take with :you. The special case is that if
  :you have an item in the boat :you have to take it with :you."
  [pos you-set-k]
  (let [items (you-set-k pos)]
    (if (and (= :boat you-set-k) (= 2 (count items)))
      (disj items :you)
      items)))

(defn next-possible-positions
  "Returns the next possible positions from the given position"
  [pos]
  (let [you-set-k (where-are-you pos)]
    (for [to-set-k (adjacent-sets you-set-k)
          item (items-with-you pos you-set-k)]
      (cond-> (move-thing pos :you you-set-k to-set-k)
        (not= item :you) (move-thing item you-set-k to-set-k)))))

(defn safe-side?
  [side]
  (or (:you side)
      (and
        (not (sets/subset? #{:goose :corn} side))
        (not (sets/subset? #{:goose :fox} side)))))

(defn safe?
  [pos]
  (and (safe-side? (:near pos))
       (safe-side? (:far pos))))

(defn next-safe-positions
  "Returns the next safe positions from the given position"
  [pos]
  (filter safe? (next-possible-positions pos)))

(def start-pos {:near #{:you :fox :goose :corn}
                :boat #{}
                :far #{}})

(def end-pos {:near #{}
              :boat #{}
              :far #{:you :fox :goose :corn}})

(defn bfs
  "Breadth first search from the given start position of the graph of safe
  positions. Returns a map that maps a position to the position that preceeded
  it in the search."
  ([start-pos] (bfs (conj (PersistentQueue/EMPTY) start-pos) {start-pos nil}))
  ([queue tree]
   (if (not-empty queue)
     (let [head (peek queue)
           queue (pop queue)
           unvisited-adj (remove #(contains? tree %) (next-safe-positions head))]
       (recur (into queue unvisited-adj)
              (reduce #(assoc % %2 head)
                      tree
                      unvisited-adj)))
     tree)))

(defn path-from-node-to-root
  "Given the output from `bfs` and a path with a position in it returns the
  path taken in the search from the given position back to the start position."
  ([tree [h :as path]]
   (let [parent (get tree h)]
     (if parent
       (recur tree (cons parent path))
       path))))

(defn to-wonderland-pos
  "Formats a position to be the same structure as those expected by the tests."
  [pos]
  [(vec (:near pos))
   (vec (cons :boat (seq (:boat pos))))
   (vec (:far pos))])

(defn river-crossing-plan
  []
  (map to-wonderland-pos (path-from-node-to-root (bfs start-pos) [end-pos])))

