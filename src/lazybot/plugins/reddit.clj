(ns lazybot.plugins.reddit
  (:require [lazybot.plugins.http-info :as http-info]
            [clojure.java.io :as io]
            [named-re.core]
            [clj-http.client :as http]
            [clj-time.format :as f]
            [clojure.string :as string]))

(def api-base "http://www.reddit.com/")
(def user-agent "jvm:lazybot:v1 (by TronPaul http://github.com/TronPaul/lazybot)")

(def link-pattern #"/(?<type>[^/]+)/(?<target>[^?/]+)(/comments/(?<longid>(?<postid>[^/]+)/(?<postname>[^/]+)(/(?<commentid>[^/]+))?)?)?")

(defn api-request [url & [req]]
  (http/get url (merge {:headers {"User-Agent" user-agent}} req)))

(defn parse-link [url]
  (re-find link-pattern (.getPath url)))

(defn handle-user [user]
  (let [api (str api-base "user/" user "/about.json")
        info (-> (api-request api) :body http-info/parse-clj-string :data)]
    (str http-info/bold (:name info) http-info/bold " - " http-info/bold (:link_karma info) http-info/bold " Link Karma - "
         http-info/bold (:comment_karma info) http-info/bold " Comment Karma - Joined " (f/unparse (f/formatters :basic-date) (f/parse (:created_utc info))))))

(defn handle-subreddit [subreddit]
  (let [api (str api-base "r/" subreddit "/about.json")
        info (-> (api-request api) :body http-info/parse-clj-string :data)]
    (str (when (:over_18 info)
           "[NSFW] ") (:url info) ": " http-info/bold (:title info) http-info/bold
         " - " http-info/bold (:subscribers info) http-info/bold " subscribers - "
         (subs (:public_description info) 0 (min 512 (count (:public_description info)))))))

(defn handle-post [post]
  (let [api (str api-base "comments/" post ".json")
        info (-> (api-request api) :body http-info/parse-clj-string first :data :children first :data)]
    (str (when (:over_18 info)
           "[NSFW] ") "/r/" (:subreddit info) ": " http-info/bold (:title info) http-info/bold " - "
         http-info/bold (:score info) http-info/bold " Karma - " http-info/bold (:num_comments info) http-info/bold " Comments")))

(defn handle-comment [comment]
  (let [api (str api-base "comments/" comment ".json")
        info (-> (api-request api {:query-params {"depth" 1}}) :body http-info/parse-clj-string second :data :children first :data)
        score (- (int (:ups info)) (int (:downs info)))
        attrs (filter identity [(when (> (:gilded info) 0) "Gilded") (when (:edited info) "Edited")])
        body (first (string/split (:body info) #"\n"))]
    (str (when (:over_18 info)
           "[NSFW] ") "Comment by " (:author info) " " http-info/bold score http-info/bold " Karma " (when (> (count attrs) 0)
                                                                                                            (str "(" (string/join attrs ",") ") ")) "- " body)))

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