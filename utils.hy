(defn list? [x]
  "Convenience function to tell if x is a list."
  (isinstance x list))

(defn third [x] (nth x 2))
(defn fourth [x] (nth x 3))
(defn fifth [x] (nth x 4))
(defn sixth [x] (nth x 5))
(defn seventh [x] (nth x 6))
(defn eighth [x] (nth x 7))
(defn ninth [x] (nth x 8))
(defn tenth [x] (nth x 9))

(defn readline-socket [socket]
  "A generator for conveniently iterating over the input from a
  socket, if any."
  
  (try
   (setv buffer (.decode (socket.recv 4096) "utf-8"))
   (except [e Exception]
     (setv buffer "")))
  (setv buffering true)
  
  (while buffering
    (if (in "\n" buffer)
      (do
       (setv (, line buffer) (buffer.split "\n" 1))
       (yield line))
      (do
       (try (setv more (.decode (socket.recv 4096) "utf-8"))
            (except [e Exception]
              (setv more nil)))
       (if (not more)
         (setv buffering false)
         (setv buffer (+ buffer more))))))
            
  (if buffer
    (yield buffer)))
