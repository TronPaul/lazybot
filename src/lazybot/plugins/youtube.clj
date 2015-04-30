(ns lazybot.plugins.youtube
  (:require [cheshire.core :refer [parse-string]]
            [lazybot.plugins.http-info :as http-info]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def api-url "https://www.googleapis.com/youtube/v3/videos")

(defn api-key [bot]
  (-> bot :config :youtube :api-key))

(defn get-video-id [link]
  (let [url (io/as-url link)]
    (if (contains? ["youtu.be" "www.youtu.be"] (.getHost url))
      (let [path (.getPath url)]
        (first (string/split (subs path 1 (- (count path) 1)) #"&")))
      (if-let [q-param (first (filter #(= (subs % 0 2) "v=") (string/split (.getQuery url) #"&")))]
        (last (string/split q-param #"="))))))

(defn get-duration [dstr]
  (let [[_ _ minutes _ seconds] (re-find #"^PT((?<minutes>\d+)M)?((?<seconds>\d+)S)?$" dstr)]
    (str (when minutes (str minutes "\u0002m\u0002")) seconds "\u0002s\u0002")))

(defn parse-clj-string [s]
  (parse-string s true))

(defn get-video-info [id key]
  (let [info (-> (http/get api-url {:query-params {:id id :key key :part "contentDetails,statistics,snippet"}}) :body parse-clj-string :items first)]
    {:Youtube  (-> info :snippet :title)
     :By       (-> info :snippet :channelTitle)
     :Views    (-> info :statistics :viewCount)
     :Duration (get-duration (-> info :contentDetails :duration))
     :Likes    (-> info :statistics :likeCount)
     :Dislikes (-> info :statistics :dislikeCount)}))

(http-info/defurlhandler
  (:url
    #"(https?://)?(youtu\.be/.+|(www\.)?youtube\.com/.+[?&]v=.*)"
    (fn [{:keys [bot] :as com-m} link verbose?]
      (if-let [vid (get-video-id link)]
        (string/join " " (doall (map (fn [[k v]]
                                       (str "\u0002" (name k) ":\u0002 " v)) (get-video-info vid (api-key @bot)))))))))
