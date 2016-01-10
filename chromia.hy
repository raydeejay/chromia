#!/usr/bin/env hy

(import config
        [irc [IRCSession]])

(def bot (IRCSession config))

;; (setv tail-line [])
;; (for [(, i tail) (enumerate config.tail-files)]
;;   (tail-line.append ""))

(defmain [&rest args]
  (bot.start)
  (bot.login)
  (bot.mainloop))
