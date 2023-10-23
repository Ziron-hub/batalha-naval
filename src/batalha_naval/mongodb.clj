(ns batalha-naval.mongodb
    (:require [monger.core :as mg]
              [monger.collection :as mc]))

(defn establish-connection []
  (let [conn (mg/connect)
        db (mg/get-db conn "batalha-naval")]
    [conn db]))

(defn create-player [nickname n-tentativas tempo [conn db]]
  (mc/insert db
             :players
             {:nickname nickname
              :n-tentativas n-tentativas
              :tempo tempo}))

(defn update-player [nickname n-tentativas tempo [conn db]]
  (mc/update db
             :players
             {:nickname nickname}
             {:nickname nickname
              :n-tentativas n-tentativas
              :tempo tempo}))

(defn get-player-exists [nickname [conn db]]
  (let [results (mc/find-one db "players" {:nickname {:$eq nickname}})]
    (not (nil? results))))
