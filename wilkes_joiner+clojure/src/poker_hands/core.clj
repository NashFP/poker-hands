(ns poker-hands.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as string]))

(def data-file "../data/p054_poker.txt")

(defn parse-rank [rank]
  (let [special {"T" "10"
                 "J" "11"
                 "Q" "12"
                 "K" "13"
                 "A" "14"}]
    (edn/read-string (get special rank rank))))

(defn parse-card [[rank suite]]
  [(-> rank str parse-rank)
   (-> suite str keyword)])

(defn parse-hands [s]
  (map set
       (partition 5
                  (map parse-card (string/split s #"\s")))))

(defn read-hands [file-path]
  (with-open [data (io/reader file-path)]
    (->> data
         line-seq
         (map parse-hands)
         doall)))

(defn ->ranks [hand]
  (map first hand))

(defn ->suits [hand]
  (map second hand))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (->ranks hand))))
             4))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (->ranks hand))))
             3))

(defn count-pairs [hand]
  (reduce (fn [pair-count [r n]]
            (if (= 2 n)
              (inc pair-count)
              pair-count))
          0
          (frequencies (->ranks hand))))

(s/def ::rank (set (range 2 (inc 14))))
(s/def ::suit #{:S :H :C :D})
(s/def ::card (s/tuple ::rank ::suit))
(s/def ::hand (s/coll-of ::card
                         :kind set?
                         :count 5))
;Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
(s/def ::royal-flush (s/and ::hand
                            ::straight-flush
                            #(= 10 (reduce min (->ranks %)))))
;Straight Flush: All cards are consecutive values of same suit.
(s/def ::straight-flush (s/and ::hand ::straight ::flush))
;Four of a Kind: Four cards of the same value.
(s/def ::four-of-a-kind (s/and ::hand four-of-a-kind?))
;Full House: Three of a kind and a pair.
(s/def ::full-house (s/and ::hand ::three-of-a-kind ::one-pair))
;Flush: All cards of the same suit.
(s/def ::flush (s/and ::hand #(= 1 (count (set (->suits %))))))
;Straight: All cards are consecutive values.
(s/def ::straight (s/and ::hand
                         #(= 5 (count (set (->ranks %))))
                         #(= (set (->ranks %))
                             (set (range (reduce min (set (->ranks %)))
                                         (inc (reduce max (set (->ranks %)))))))))
;Three of a Kind: Three cards of the same value.
(s/def ::three-of-a-kind (s/and ::hand three-of-a-kind?))
;Two Pairs: Two different pairs.
(s/def ::two-pairs (s/and ::hand #(= 2 (count-pairs %))))
;One Pair: Two cards of the same value.
(s/def ::one-pair (s/and ::hand #(= 1 (count-pairs %))))

(s/def ::ranked-hand (s/and ::hand
                            (s/or :royal-flush ::royal-flush
                                  :straight-flush ::straight-flush
                                  :four-of-a-kind ::four-of-a-kind
                                  :full-house ::full-house
                                  :flush ::flush
                                  :straight ::straight
                                  :three-of-a-kind ::three-of-a-kind
                                  :two-pairs ::two-pairs
                                  :one-pair ::one-pair
                                  :high-card any?)))

(def hand-score {:royal-flush 9
                 :straight-flush 8
                 :four-of-a-kind 7
                 :full-house 6
                 :flush 5
                 :straight 4
                 :three-of-a-kind 3
                 :two-pairs 2
                 :one-pair 1
                 :high-card 0})

(defn score-high-card [left right]
  (let [max-left (reduce max (->ranks left))
        max-right (reduce max (->ranks right))]
    (compare max-left max-right)))

(defn compare-hands [left right]
  (let [left-score (hand-score (first (s/conform ::ranked-hand left)))
        right-score (hand-score (first (s/conform ::ranked-hand right)))
        winner (compare left-score right-score)
        winner (if (zero? winner)
                 (if (zero? left-score)
                   (score-high-card left right)
                   0)
                 winner)]
    {:winner ({1 :left 0 :tie -1 :right} winner)
     :left (s/conform ::ranked-hand left)
     :right (s/conform ::ranked-hand right)}))

(s/fdef compare-hands
        :args (s/cat :left ::hand :right ::hand)
        :ret any?)

(comment
  (s/exercise-fn `compare-hands)

  (def results
    (map #(apply compare-hands %) (read-hands data-file)))
  :end)