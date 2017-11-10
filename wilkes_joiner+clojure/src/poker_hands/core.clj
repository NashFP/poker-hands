(ns poker-hands.core
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def s "TH 8H 5C QS TC 9H 4D JC KS JS")

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
  (partition 5
             (map parse-card (string/split s #"\s"))))

(defn read-hands [file-path]
  (with-open [data (io/reader file-path)]
    (->> data
         line-seq
         (map parse-hands)
         doall)))