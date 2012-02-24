;;; -*- Mode: Lisp -*-

(defpackage #:rtorrent-controller
  (:use #:cl #:qt #:qt-ui)
  (:export
   #:inotify-loop
   #:gui))
