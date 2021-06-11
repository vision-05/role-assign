(ns role-assign.core
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.string]
            [clojure.set]
            [clojure.core.async :refer [chan close!]]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]]))

(def state (atom nil))

(def bot-id (atom nil))

(def config (edn/read-string (slurp "config.edn")))

(defn remove-last [string-arg]
  (clojure.string/reverse (subs (clojure.string/reverse string-arg) 1)))

(defn get-id-raw [id]
  (remove-last (subs id 3)))

(defn parse-id [id]
  (if (= (subs id 0 1) "<") (get-id-raw id)
      id))

(defmulti handle-event (fn [type _data] type))

(defmethod handle-event :message-create [event-type {{bot :bot} :author :keys [channel-id guild-id author content]}]
  (let [connection (:rest @state)
        message (into [] (filter #(not= % "") (clojure.string/split content #" ")))
        channel @(discord-rest/get-channel! connection channel-id)
        author-user @(discord-rest/get-guild-member! connection guild-id (:id author))]
    (if (and (= "!" (subs content 0 1)) (> (count (clojure.set/intersection (into #{} (:roles config)) (into #{} (:roles author-user)))) 0))
      (let [target-user (parse-id (message 1))
            target-role (parse-id (message 2))]
        (cond
          (= (message 0) "!give-role") @(discord-rest/add-guild-member-role! connection guild-id target-user target-role)
          (= (message 0) "!take-role") @(discord-rest/remove-guild-member-role! connection guild-id target-user target-role))))))

(defmethod handle-event :ready
  [_ _]
  (discord-ws/status-update! (:gateway @state) :activity (discord-ws/create-activity :name (:playing config))))

(defmethod handle-event :default [_ _])

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (discord-ws/disconnect-bot! gateway)
  (close! events))

(defn -main [& args]
  (reset! state (start-bot! (:token config) :guild-messages))
  (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
  (try
    (message-pump! (:events @state) handle-event)
    (finally (stop-bot! @state))))

