;;; -*- Mode: Lisp -*-

(asdf:defsystem #:rtorrent-controller
  :serial t
  :depends-on (s-xml-rpc inotify)
  :components ((:file "packages")
               (:file "rtorrent-controller")))
