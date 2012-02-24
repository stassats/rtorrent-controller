;;; -*- Mode: Lisp -*-

(asdf:defsystem #:rtorrent-controller
  :serial t
  :depends-on (s-xml-rpc inotify
                         qt-ui)
  :components ((:file "packages")
               (:file "rtorrent-controller")
               (:file "gui")))
