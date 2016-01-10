(def *commands* {
                 "!say " (lambda [bot text]
                           (let [msg (cut text (+ 5 (text.find "!say ")))]
                             (print "Saying:" msg)
                             (bot.say-to-channel (first bot.config.channels) msg)))
                 "!parse" (lambda [bot text]
                            (bot.say-to-channel (first bot.config.channels)
                                                (str (parse-message text))))
                 "!quit" (lambda [bot text]
                           (print "Quitting.")
                           (bot.quit-irc))  })
