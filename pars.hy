(defclass IRCBadMessage [Exception])

(defn parse-message [message]
  "Breaks a message from an IRC server into its prefix, command, and
  arguments."

  (if (not message)
    (raise (IRCBadMessage "Empty line.")))

  ;; strip newline chars (have to check the RFC... or not)
  (let [s (.strip message "\n\r")
        prefix ""
        trailing []]

    ;; grab the prefix if it exists
    (if (= (first s) ":")
      (setv (, prefix s) (.split (cut s 1) " " 1)))

    ;; make the args list considering if there's a "colon argument"
    (if (!= (.find s " :") -1)
      (do
       (setv (, s trailing) (.split s " :" 1)
             args (.split s))
       (.append args trailing))
      (setv args (.split s)))

    ;; extract the IRC command from the args
    (setv command (.pop args 0))

    ;; return a tuple
    (, prefix command args)))
