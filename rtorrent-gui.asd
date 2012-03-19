;;; -*- Mode: Lisp -*-

(defsystem rtorrent-gui
  :serial t
  :depends-on (rtorrent-controller
               qt-ui)
  :components ((:module "gui"
                :serial t
                :components
                ((:file "packages")
                 (:file "gui")))))
