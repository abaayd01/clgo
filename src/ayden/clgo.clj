(ns ayden.clgo
  (:gen-class)
  (:require [ayden.utils :as utils]
            [clojure.set :refer [union]]))

(def player-sequence
  "Generate an infinite lazy sequence for the player turns"
  (cycle [:b :w]))

(defn make-row [size]
  (apply vector (repeat size nil)))

(defn make-board [size]
  (apply vector (repeat size (make-row size))))

(defn board-symbol
  [v]
  (case v
    nil "+"
    :b  "@"
    :w  "O"))

(defn display-row
  [row display-spacer?]
  (let [r        (->> (map board-symbol row)
                      (interpose "---")
                      (reduce str))
        r-spacer (->> (repeat (count row) "|")
                      (interpose "   ")
                      (reduce str))]
    (println r)
    (when display-spacer?
      (println r-spacer))))

(defn display-board
  [board]
  (doseq [[i row] (utils/indexed board)]
    (display-row row (not= i (dec (count board)))))
  board)

(def init-board
  (make-board 5))

;; point methods
(defn isa-point?
  [point]
  (when (and (= (count point) 2)
             (every? int? point))
    true))

(defn up
  [[row col]]
  [(dec row) col])

(defn down
  [[row col]]
  [(inc row) col])

(defn left
  [[row col]]
  [row (dec col)])

(defn right
  [[row col]]
  [row (inc col)])

(defn surrounding-points
  [point]
  [(left point)
   (up point)
   (right point)
   (down point)])

;; board methods
(defn point-on-board?
  [board point]
  (let [board-size (count board)]
    (every? #(and (>= % 0) (< % board-size)) point)))

(defn value-at-point
  [board point]
  (if (point-on-board? board point)
    (get-in board point)
    :x))

(defn set-value-at-point
  [board point v]
  (assoc-in board point v))

(defn enemy
  [v]
  (if (= v :w)
    :b
    :w))

(defn enemy?
  [player v]
  (= (enemy player) v))

(defn friend?
  [player v]
  (= player v))

(defn enemy-at-point?
  [board point player]
  (when (point-on-board? board point)
    (enemy? player (value-at-point board point))))

(defn friend-at-point?
  [board point player]
  (when (point-on-board? board point)
    (friend? player (value-at-point board point))))

(defn visited-point?
  [point points-visited]
  (not (nil? (points-visited point))))

(defn liberties-at-point
  [board point]
  (->> (surrounding-points point)
       (filter #(nil? (value-at-point board %)))))

(defn friends-not-visited
  [board point player points-visited]
  (->> (surrounding-points point)
       (filter #(and (friend-at-point? board % player)
                     (not (visited-point? % points-visited))))))

;; group methods
(defn set-value-for-group
  [board group value]
  (reduce
    (fn [acc cur]
      (set-value-at-point acc cur value))
    board
    group))

(defn liberties-for-group
  [board points]
  (flatten (map #(liberties-at-point board %) points)))

(defn get-group-inner
  [board point player points-visited]
  (let [friends (friends-not-visited board point player points-visited)]
    (if (empty? friends)
      (conj points-visited point)
      (map
        #(get-group-inner board % player (conj points-visited point))
        friends))))

(defn get-group
  [board point player]
  (let [sets (get-group-inner board point player #{})]
    (if (seq? sets)
      (apply union (flatten sets))
      sets)))

;; game methods

;; receiving user input
(defn parse-point-input
  [s]
  (try
    (let [val (->> (clojure.string/split s #" ")
                   (map #(Integer/parseInt %))
                   (into []))]
      (if (isa-point? val)
        val
        nil))
    (catch Exception e nil)))

(defn get-input
  [board]
  (let [raw-input (read-line)]
    (if (#{"q" "quit" "pass" "resign"} raw-input)
      (keyword raw-input)
      (let [point (parse-point-input raw-input)]
        (cond
          (nil? point)                        (do (println "invalid input, try again.")
                                                  (get-input board))
          (not (point-on-board? board point)) (do (println "invalid input, point not on board.")
                                                  (get-input board))
          (value-at-point board point)        (do (println "invalid input, point already taken.")
                                                  (get-input board))
          :else                               point)))))

;; updating the board state
(defn remove-captured-stones
  [board point player]
  (let [groups-to-kill (->> (surrounding-points point)
                            (filter #(enemy-at-point? board % player))
                            (map #(get-group board % (enemy player)))
                            (filter #(empty? (liberties-for-group board %))))]

    (reduce (fn [acc group-to-kill]
              (set-value-for-group acc group-to-kill nil))
            board
            groups-to-kill)))

(defn apply-move
  [board point player]
  (-> (set-value-at-point board point player)
      (remove-captured-stones point player)))

(defn play-game
  "The game loop.
  We iterate through the player sequence (alternate player turns)
  until there is a winner or the board is full."
  [starting-board player-sequence]
  (loop [board           starting-board
         player-sequence player-sequence]
    (println "Current Turn:" (first player-sequence))
    (println "Current board:")
    (display-board board)
    (let [input  (get-input board)
          player (first player-sequence)]
      (cond
        (#{:quit :q} input) (println "exiting...\n")
        (#{:pass} input)    (do (println "passing...\n")
                                (recur board (rest player-sequence)))
        (#{:resign} input)  (println "player" player "resigned!\n")
        (some? input)       (let [next-board  (apply-move board input player)
                                  valid-move? (seq (liberties-at-point next-board input))]
                              (println "point: " input "\n")
                              (if valid-move?
                                (recur next-board (rest player-sequence))
                                (do
                                  (println "illegal move! try again..")
                                  (recur board player-sequence))))
        :else               (println "something went wrong...exiting...\n")))))

(comment
  (play-game init-board player-sequence))

(comment
  (let [board (-> init-board
                  (set-value-at-point [0 1] :b)
                  (set-value-at-point [1 1] :b)
                  (set-value-at-point [1 0] :b)
                  ;; (set-value-at-point [2 0] :w)
                  (set-value-at-point [2 1] :w)
                  (set-value-at-point [1 2] :w)
                  (set-value-at-point [0 2] :w)
                  (set-value-at-point [2 2] :w)
                  )]
    (display-board board)
    (display-board (let [next-board  (apply-move board [0 0] :w)
                         valid-move? (seq (liberties-at-point next-board [0 0]))]
                     (if valid-move?
                       (do (println "valid move")
                           next-board)
                       (do
                         (println "illegal move! try again..")
                         board)))))

  (let [board (-> init-board
                  (set-value-at-point [0 1] :b)
                  (set-value-at-point [1 1] :b)
                  (set-value-at-point [1 0] :b)
                  (set-value-at-point [2 1] :w)
                  (set-value-at-point [1 2] :w)
                  (set-value-at-point [0 2] :w)
                  (set-value-at-point [2 2] :w)
                  )]
    (display-board board)
    (display-board (apply-move board [0 0] :w)))

  (let [board (-> init-board
                  (set-value-at-point [1 1] :b)
                  (set-value-at-point [0 1] :w)
                  (set-value-at-point [1 0] :w)
                  (set-value-at-point [2 1] :w))]
    (display-board board)
    (display-board (apply-move board [1 2] :w)))
  )
