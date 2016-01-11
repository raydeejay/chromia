(import [pars [parse-message]]
        [utils [*]])


(import re)
(defreader r [expr] `(re.compile ~expr))


(defclass Command [object]
  "This is the base Command class."
  [bot nil
   text nil
   msg nil
   source nil
   issuer nil
   args nil
   public false
   unlisted false
   name nil
   patterns []]

  (defn --init-- [self bot text]
    (setv self.bot bot
          self.text text
          self.msg (parse-message text)
          self.source (first self.msg)
          self.issuer (first (third self.msg))
          self.args (second (third self.msg))))

  (defn action [self]
    "This is the default action: nothing.")

  (defn execute [self]
    "Run the command for a particular issuer with particular
    arguments."
    (self.action)))


(defclass CommandSay [Command]
  "Say something to someone.

This command will echo the whole arguments, raw, back to the place
where the command was posted to, be it a query with a user, or a
channel."
  [name "say"
   public true
   patterns [#r"^!say (.*)" #r"^!speak (.*)"]]

  (defn action [self]
    (let [source self.issuer]
      (self.bot.send-to source (second (.split self.args " " 1))))))


(defclass CommandBye [Command]
  "Commands the bot to shutdown."
  [name "bye"
   patterns [#r"^!bye$"]]

  (defn action [self]
    (let [source self.issuer]
      (self.bot.send-to source "Shutting down.")
      (self.bot.stop))))


(defclass CommandCommands [Command]
  "Lists the available commands, with a brief description."
  [name "commands"
   patterns [#r"^!commands$"]]

  (defn action [self]
    (let [source self.issuer]
      (for [cls cmdlist]
        (self.bot.send-to source (% "%s - %s"
                                           (, cls.--name--
                                              (first (.split cls.--doc-- "\n" 1)))))))))


(defclass CommandHelp [Command]
  "Display the full documentation of a command."
  [name "help"
   patterns [#r"^!help (.*)$"]]

  (defn action [self]
    (let [source self.issuer
          wanted-command (second (.split self.args " " 1))]
      (for [cls cmdlist]
        (when (= cls.name wanted-command)
          (for [line (flatten [cls.--name--
                               (.split cls.--doc-- "\n")])]
            (self.bot.send-to source line)))))))


(defclass CommandAttack [Command]
  "Cause some pain to someone."
  [name "attack"
   patterns [#r"^\.attack (.*)$"]]

  (defn action [self]
    (let [source self.issuer
          attacker (first (.split source "!" 1))
          victim (second (.split self.args " " 1))]
      (self.bot.send-to source (% "%s hits %s for 78546 points of damage."
                                  (, attacker
                                     victim))))))


(def cmdlist (.--subclasses-- Command))

;; FIXME - should this be a method on Command?
(defn interpret [bot text]
  "Tries to find a command matching the incoming IRC message, and if
  it does, executes it."

  (for [cls cmdlist]
    (for [pattern cls.patterns]
      (let [match (pattern.match (second (third (parse-message text))))]
        (when match
          (.execute (cls bot text)))))))
