;;; -*- Mode: Lisp -*-

(defsystem rtorrent-controller
  :serial t
  :depends-on (s-xml-rpc #+linux inotify
                         #+darwin "kqueue")
  :components ((:file "packages")
               (:file "rtorrent-controller")))

