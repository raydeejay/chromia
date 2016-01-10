(import ssl socket time
        [utils [*]])

(defclass IRCBadMessage [Exception])

(defn parse-message [s]
  "Breaks a message from an IRC server into its prefix, command, and
  arguments."

  (setv prefix ""
        trailing [])

  (if (not s)
    (raise (IRCBadMessage "Empty line.")))

  (setv s (.strip s "\n\r"))

  (if (= (first s) ":")
    (setv (, prefix s) (.split (cut s 1) " " 1)))

  (if (!= (.find s " :") -1)
    (do
     (setv (, s trailing) (.split s " :" 1)
           args (.split s))
     (.append args trailing))
    (setv args (.split s)))

  (setv command (.pop args 0))
  (, prefix command args))

(defn irc-send [socket message]
  (.send socket (.encode message "utf-8")))

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

  (defn quit-irc [self]
    (setv self.state STOP))

  (defn join-channels [self]
    (for [c self.config.channels]
      (join-channel self.socket c)))

  (defn say-to-channel [self channel message]
    (.send self.socket
           (.encode (+ "PRIVMSG " channel " :" message "\n")
                    "utf-8")))

  (defn start [self]
    (print (% "Establishing connection to [%s]" (, self.config.server)))
    (self.socket.connect (, self.config.server self.config.port))
    (self.socket.setblocking false))

  (defn login [self]
    (print (% "Logging in as %s" (, self.config.botnick)))
    (irc-send self.socket (+ "USER " self.config.botnick " 0 * :A Hy bot\n"))
    (if self.config.server-password
      (irc-send self.socket (% "PASS %s\n" (, self.config.password)))
    (irc-send self.socket (+ "NICK " self.config.botnick "\n"))))
  
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
           (print text)

           ;; Prevent Timeout
           (when (!= (text.find "PING") -1)
             (print "Replying to ping...")
             (irc-send self.socket (+ "PONG " (. (text.split) [1]) "\r\n")))

           ;; join channels
           (when (!= (text.find "End of /MOTD command.") -1)
             (print "Logging in to Nickserv...")
             (irc-send self.socket (% "PRIVMSG nickserv :identify %s %s\r\n" (, self.config.botnick self.config.password)))
             (print "Logged in, joining channels...")
             (self.join-channels))

           ;; say
           (when (!= (text.find "!say ") -1)
             (let [msg (cut text (+ 5 (text.find "!say ")))]
               (print "Saying:" msg)
               (self.say-to-channel (first self.config.channels) msg)))

           ;; reload
           (when (!= (text.find "!reload") -1)
             (let [m "Reloading..."]
               (print m)
               (self.say-to-channel (first self.config.channels) m)))

           ;; parse
           (when (!= (text.find "!parse") -1)
             (self.say-to-channel (first self.config.channels) (str (parse-message text))))

           ;; quit
           (when (!= (text.find "!quit") -1)
             (print "Quitting.")
             (self.quit-irc)))

         (except [e Exception]
           (print (% "Error with input %s" (, text)))
           (print e)
           (continue)))))

    (when (= self.state STOP)
      (self.say-to-channel (first self.config.channels) "Stopping.")
      (irc-send self.socket "QUIT \n")))    

  (defn stop [self])

  (defn send-to [self])

  (defn send-to-channel [self])

  (defn send-to-admin [self])

  (defn send-to-target [self]))
