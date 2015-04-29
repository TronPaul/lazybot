(ns lazybot.plugins.youtube
  (:require [lazybot.registry :as registry]
            [lazybot.plugins.http-info :as http-info]))

(http-info/defurlhandler
  (:url
    #"(https?://)?(youtu\.be/.+|(www\.)?youtube\.com/.+[?&]v=.*)"
    (fn [com-m link verbose?]
      link)))
