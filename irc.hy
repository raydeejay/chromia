(import ssl socket time
        [reloader [hy-reload :as reload]]
        [utils [*]]
        commands)

(import [pars [parse-message]])

(defn irc-send [socket message]
  (.send socket (.encode (+ message "\r\n") "utf-8")))

(defn join-channel [socket channel]
  (irc-send socket (+ "JOIN " channel "\n")))

;; state flags
(setv START 1
      RESTART 90
      STOP 99)
(def *state* START)



(defclass IRCSession [object]
  "This is the class!"
  [socket nil
   state nil
   config nil]

  (defn --init-- [self config]
    (let [c-socket (socket.socket socket.AF-INET socket.SOCK-STREAM)]
      (setv self.socket (ssl.wrap-socket c-socket)
            self.state START
            self.config config)))

  (defn join-channels [self]
    (for [c self.config.channels]
      (join-channel self.socket c)))

  (defn send-to [self target message]
    (irc-send self.socket(+ "PRIVMSG " target " :" message)))

  (defn send-to-admin [self] "TBI")

  (defn send-to-reporting-channels [self] "TBI")

  (defn start [self]
    (print (% "Establishing connection to [%s]" (, self.config.server)))
    (self.socket.connect (, self.config.server self.config.port))
    (self.socket.setblocking false))

  (defn stop [self]
    (print (% "Disconnecting from [%s]" (, self.config.server)))
    (setv self.state STOP))

  (defn login [self]
    (print (% "Logging in as %s" (, self.config.botnick)))
    (irc-send self.socket (+ "USER " self.config.botnick " 0 * :" self.config.description))
    (if self.config.server-password
      (irc-send self.socket (% "PASS %s" (, self.config.password))))
    (irc-send self.socket (+ "NICK " self.config.botnick)))

  (defn mainloop [self]
    (while (< self.state STOP)
      (time.sleep 0.001)

      ;; Tail Files
      ;; (for [(, i tail) (enumerate config.tail-files)]
      ;;   (try
      ;;    (do
      ;;     (with [f (open tail "r")]
      ;;           (setv line
      ;;                 (. (f.readlines) [-1])))
      ;;     (when (!=  (. tail-line [i]) line)
      ;;       (setv (. tail-line [i]) line)
      ;;       (let [channel (first config.channels)]
      ;;         (irc-send self.socket (% "PRIVMSG %s :%s" (, channel line))))))
      ;;    (except [e Exception]
      ;;      (print (% "Error with file %s" (, tail)))
      ;;      (print e))))

      ;; process input
      (for [text (readline-socket self.socket)]
        (try
         (when text
           (let [irc-message (parse-message text)
                 irc-issuer (first irc-message)
                 irc-command (second irc-message)
                 irc-args (third irc-message)]
             (print text)
             (print irc-message)

             ;;; AUTONOMOUS RESPONSES
             ;; Prevent Timeout
             (when (= irc-command "PING")
               (print "Replying to ping...")
               (irc-send self.socket (+ "PONG " (first irc-args))))

             ;; join channels
             (when (= irc-command "376")
               (print "End of MOTD, logging in to Nickserv...")
               (irc-send self.socket (% "PRIVMSG nickserv :identify %s %s" (, self.config.botnick self.config.password)))
               (print "Logged in to IRC, joining channels...")
               (self.join-channels))

             ;;; BUILTINS
             ;; let's leave here just in case, for now
             (when (!= (text.find "!reload") -1)
               (let [m "Reloading commands."
                     message (parse-message text)
                     chan (first irc-args)]
                 (print m)
                 (self.send-to chan m))
               (reload "commands"))

             ;;; COMMANDS
             ;; some rudimentary guard
             (when (and (= irc-command "PRIVMSG")
                        (in (first irc-args)
                            ["#gossip" "#AppliedEnergistics" self.config.botnick]))
               ;; class-based commands
               (commands.interpret self text))))

         (except [e Exception]
           (print (% "Error with input %s" (, text)))
           (print e)
           (continue)))))

    (when (= self.state STOP)
      (self.send-to (first self.config.channels) "Stopping.")
      (irc-send self.socket "QUIT"))))
