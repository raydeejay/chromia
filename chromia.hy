#!/usr/bin/env hy

(import socket ssl time)

(defn list? [x]
  "Convenience function to tell if x is a list."
  (isinstance x list))

;; IRC Settings
(setv server "irc.esper.net"
      port 6697
      channels "#gossip"
      botnick "chromia2"
      password "YOURPASSWORD")

;; running flag
(def *run* true)

;;; Tail
(def tail-files ["/tmp/file-to-tail.txt"])

;; defines the socket
(setv irc-C (socket.socket socket.AF-INET socket.SOCK-STREAM)
      irc (ssl.wrap-socket irc-C))

;; Commands
(defn irc-send [session message]
  (.send session (.encode message "utf-8")))

(defn join-channels []
  (for [c (list? channels channels [channels])]
    (irc-send irc (+ "JOIN " c "\n"))))

(defn quit-irc []
  (irc-send irc (+ "QUIT " channel "\n"))
  (global *run*)
  (set *run* false))

(defn say-to-channel [session message]
  (.send session (.encode (+ "PRIVMSG " channel " :" message) "utf-8")))

;; Connect
(print (% "Establishing connection to [%s]" (, server)))
(irc.connect (, server port))
(irc.setblocking false)
;; (irc-send irc (% "PASS %s\n" (, password)))
(irc-send irc (+ "USER " botnick " " botnick " " botnick " :what-is-this\n"))
(irc-send irc (+ "NICK " botnick "\n"))
;; (irc-send irc (% "PRIVMSG nickserv :identify %s %s\r\n" (, botnick password)))


(setv tail-line [])
(for [(, i tail) (enumerate tail-files)]
  (tail-line.append ""))

(defn run []
  (global *run*)
  (setv *run* true)
  
  (while *run*
    (time.sleep 2)

    ;; Tail Files
    (for [(, i tail) (enumerate tail-files)]
      (try
       (do
        (setv f (open tail  "r")
              line (. (f.readlines) [-1]))
        (f.close)
        (when (!=  (. tail-line [i]) line)
          (setv (. tail-line [i]) line)
          (irc-send irc (% "PRIVMSG %s :%s" (, channel line))))
        )
       (except [e Exception]
         (print (% "Error with file %s" (, tail)))
         (print e))))

    ;; process input
    (try
     (do
      (setv text (.decode (irc.recv 2040) "utf-8"))
      (print text)

      ;; Prevent Timeout
      (when (!= (text.find "PING") -1)
        (print "Replying to ping...")
        (irc-send irc (+ "PONG " (. (text.split) [1]) "\r\n")))

      ;; join channel
      (when (!= (text.find "End of /MOTD command.") -1)
        (print "Logged in, joining channels...")
        (join-channels))

      ;; say
      (when (!= (text.find "!say ") -1)
        (print "Saying: " text)
        (let [msg (cut text (+ 5 (text.find "!say ")))]
          (say irc msg)))

      ;; quit
      (when (!= (text.find "!quit") -1)
        (print "Quitting.")
        (quit-irc))

      )
     
     (except [e Exception]
       (print (% "Error with input %s" (, text)))
       (print e)
       (continue)))))

(defmain [&rest args]
  (run))
