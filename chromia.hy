#!/usr/bin/env hy

(import config
        [irc [IRCSession]])

(def bot (IRCSession config))

;; (setv tail-line [])
;; (for [(, i tail) (enumerate config.tail-files)]
;;   (tail-line.append ""))

;; (defmacro defcommand [cmdname &body body]
;;   `(lambda [socket text]
;;      ))

;; (defcommand say
;;   ["^!say (.*)"]
;;   "Says something."
;;   (print "Saying:" msg)
;;   (say-to-channel socket channel (full-args msg)))

(defmain [&rest args]
  (bot.start)
  (bot.login)
  (bot.mainloop))
