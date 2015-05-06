(ns lazybot.plugins.reddit
  (:require [lazybot.plugins.http-info :as http-info]
            [clojure.java.io :as io]
            [named-re.core]
            [clj-http.client :as http]
            [clj-time.format :as f]))

(def api-base "http://www.reddit.com/")

(def link-pattern #"/(?<type>[^/]+)/(?<target>[^?/]+)(/comments/(?<longid>(?<postid>[^/]+)/(?<postname>[^/]+)(/(?<commentid>[^/]+))?)?)?")

(defn parse-link [url]
  (re-find link-pattern (.getPath url)))

(defn handle-user [user]
  (println (str api-base "user/" user "/about.json"))
  (let [api (str api-base "user/" user "/about.json")
        info (-> (http/get api) :body http-info/parse-clj-string :data)]
    (println info)
    (str http-info/bold (:name info) http-info/bold " - " http-info/bold (:link_karma info) http-info/bold " Link Karma - "
         http-info/bold (:comment_karma info) http-info/bold " Comment Karma - Joined " (f/unparse (f/formatters :basic-date) (f/parse (:created_utc info))))))

(defn handle-subreddit [subreddit]
  )

(defn handle-post [post]
  )

(defn handle-comment [comment]
  )

(defn handle-r-type [match]
  (cond
    (:commentid match) (handle-comment (:longid match))
    (:postid match) (handle-post (:postid match))
    :else (handle-subreddit (:target match))))

(http-info/defurlhandler
  (:url
    #"(https?://)?((www|pay)\.)?redd(it|id\.com).+"
    (fn [{:keys [bot] :as com-m} link verbose?]
      (let [url (io/as-url link)]
        (if-let [match (parse-link url)]
          (cond
            (contains? #{"u" "user"} (:type match)) (handle-user (:target match))
            (= (:type match) "r") (handle-r-type match)
            :else (handle-post (.getPath url)))
          (handle-post (.getPath url)))))))