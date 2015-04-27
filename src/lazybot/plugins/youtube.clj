(ns lazybot.plugins.youtube
  (:require [lazybot.registry :as registry]
            [lazybot.plugins.http-info :as http-info]))

(http-info/defurlhandler
  (:url
    #"(youtu\.be/.+|(www\.)?youtube\.com/.+[?&]v=.*"
    (fn [url config]
      )))
