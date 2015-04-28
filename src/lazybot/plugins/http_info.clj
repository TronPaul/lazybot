;; The result of a team effort between programble and Rayne.
(ns lazybot.plugins.http-info
  (:require [lazybot.info :as info]
            [lazybot.registry :as registry]
            [lazybot.utilities :as utilities]
            [clojure.java.io :refer [reader]]
            [clojure.string :refer [triml]]
            [clojure.tools.logging :refer [debug]]
            [clojail.core :refer [thunk-timeout]])
  (:import java.util.concurrent.TimeoutException
           org.apache.commons.lang.StringEscapeUtils))

(def titlere #"(?i)<title>([^<]+)</title>")

(defn collapse-whitespace [s]
  (->> s (.split #"\s+") (interpose " ") (apply str)))

(defn add-url-prefix [url]
  (if-not (.startsWith url "http")
    (str "http://" url)
    url))

(defn slurp-or-default [url]
  (try
   (with-open [readerurl (reader url)]
     (loop [acc [] lines (line-seq readerurl)]
       (cond
        (not (seq lines)) nil
        (some #(re-find #"</title>|</TITLE>" %) acc) (->> acc (apply str)
                                                          (#(.replace % "\n" " "))
                                                          (re-find titlere))
        :else (recur (conj acc (first lines)) (rest lines)))))
   (catch java.lang.Exception e nil)))

(defn url-blacklist-words [network bot] (:url-blacklist ((:config @bot) network)))

(defn url-check [network bot url]
  (some #(.contains url %) (url-blacklist-words network bot)))

(defn strip-tilde [s] (apply str (remove #{\~} s)))

(defn title [{:keys [network nick bot user channel] :as com-m}
             link verbose?]
  (try
    (thunk-timeout #(let [url (add-url-prefix link)
                          page (slurp-or-default url)
                          match (second page)]
                     (if (and (seq page) (seq match) (not (url-check network bot url)))
                       (registry/send-message com-m
                                              (str "\""
                                                   (triml
                                                     (StringEscapeUtils/unescapeHtml
                                                       (collapse-whitespace match)))
                                                   "\""))
                       (when verbose? (registry/send-message com-m "Page has no title."))))
                   20 :sec)
    (catch TimeoutException _
      (when verbose?
        (registry/send-message com-m "It's taking too long to find the title. I'm giving up.")))))

(defn http-info [{:keys [network nick bot user channel] :as com-m}
             links & {verbose? :verbose?}]
  (if (or (and verbose? (seq links))
          (not (contains? (get-in @bot [:config network :title :blacklist])
                          channel)))
    (doseq [link (take 1 links)]
      (title com-m link verbose?))
    (when verbose? (registry/send-message com-m "Which page?"))))

(defn parse-fns [body]
  (apply registry/merge-with-conj
         (for [[one & [two three :as args]] body]
           {one
            (case
              one
              :url {two {:fn three}}
              two)})))

(defn load-url-handler
  "Load a plugin (a Clojure source file)."
  [irc refzors plugin]
  (let [ns (symbol (str "lazybot.plugins." plugin))]
    (require ns :reload)
    ((resolve (symbol (str ns "/load-this-url"))) irc refzors)))

(defn load-url-handlers
  "Load all plugins specified in the bot's configuration."
  [irc refzors]
  (let [url-handlers (-> @refzors :config (get (:network @irc)) :urlhandlers)]
    (doseq [handler url-handlers]
      (load-url-handler irc refzors handler))))

(defmacro defurlhandler [& body]
  (let [{:keys [url]} (parse-fns body)]
    `(let [pns# *ns*
           m-name# (module-name pns#)]
       (defn ~'load-this-url [com# bot#]
         (dosync
           (alter bot# assoc-in [:url-handlers m-name#]
                  {:urls (into {}
                               (for [[k# v#] (apply registry/merge-with-conj
                                                    (registry/make-vector ~url))]
                                 [k# (registry/make-vector v#)]))}))))))

(registry/defplugin
  (:init
    (fn [com bot]
      (load-url-handlers com bot)))
  (:hook
   :privmsg
   (fn [{:keys [network bot nick channel message] :as com-m}]
     (let [info (:config @bot)
           get-links (fn [s]
                       (->> s
                            (re-seq #"(https?://|www\.)[^\]\[(){}\"'$^\s]+")
                            (map first)))]
       (let [prepend (:prepends info)
             links (get-links message)
             title-links? (and (not (registry/is-command? message prepend))
                               (get-in info [network :title :automatic?])
                               (seq links))]
         (when title-links?
           (http-info com-m links))))))

  (:cmd
   "Gets the title of a web page. Takes a link. This is verbose, and prints error messages."
   #{"title"} (fn [com-m] (http-info com-m (:args com-m) :verbose? true))))
