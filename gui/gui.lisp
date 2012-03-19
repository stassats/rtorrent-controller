;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:rtorrent-gui)
(named-readtables:in-readtable :qt)

(defvar *qapp* nil)

(defmacro with-sbcl-float-traps (&body body)
  `(#+sbcl ,@'(sb-int:with-float-traps-masked (:invalid :divide-by-zero))
    #-sbcl progn
    ,@body))

(defun gui ()
  (unless *qapp*
    (setf *qapp* (make-qapplication)))
  (let ((window (make-instance 'main-window)))
    (unwind-protect
         (with-sbcl-float-traps
           (#_show window)
           (#_exec *qapp*))
      (progn
        (when (timer window)
          (#_stop (timer window)))
        (#_hide window)))))

(defclass torrent-list (list-widget)
  ()
  (:metaclass qt-class)
  (:qt-superclass "QTreeView")
  (:slots
   ("startTorrent()" start-torrent)
   ("stopTorrent()" stop-torrent)
   ("removeTorrent()" remove-torrent))
  (:default-initargs
   :header '("Name" "Left to download" "Status")))

(defclass main-window ()
  ((torrents :initform nil
             :accessor torrents)
   (torrent-list :initform nil
                :accessor torrent-list)
   (status-fields :initarg :status-fields
                  :initform nil
                  :accessor status-fields)
   (timer :initform nil
          :accessor timer))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots ("search(QString)" search-torrent)
          ("statusUpdate()" status-update)
          ("changeMaxUp(int)" change-max-up)
          ("changeMaxDown(int)" change-max-down)))

(defun list-torrents-humanly ()
  (process-lists
   '(2 format-bytes
     3 state-description
     4 format-bytes)
   (list-torrents)))

(defun add-permanent-widgets (status-bar &rest widgets)
  (loop for widget in widgets
        do (#_addPermanentWidget status-bar widget)))

(defun add-status-bar (window)
  (let* ((status-bar (#_new QStatusBar))
         (max-up (#_new QSpinBox))
         (max-down (#_new QSpinBox))
         (timer (timer window)))
    (setf (status-fields window)
          (list :up-rate (#_new QLabel)
                :max-up max-up
                :down-rate (#_new QLabel)
                :max-down max-down))

    (#_setSingleStep max-up 50)
    (#_setSingleStep max-down 50)
    (#_setMaximum max-up 10000)
    (#_setMaximum max-down 10000)

    (connect max-up "valueChanged(int)" window "changeMaxUp(int)")
    (connect max-down "valueChanged(int)" window "changeMaxDown(int)")
    (loop for (nil widget) on (status-fields window) by #'cddr
          do (add-permanent-widgets status-bar widget))

    (connect timer "timeout()" window (qslot "statusUpdate()"))
    (#_start timer 1000)
    status-bar))

(defun status-update (window)
  (destructuring-bind (&key up-rate max-up down-rate max-down)
      (status-fields window)
    (with-signals-blocked (max-up max-down)
        (destructuring-bind (v-up-rate v-down-rate v-max-up v-max-down)
            (status)
          (#_setText up-rate
                     (format nil "U: ~10@a" (format-bytes v-up-rate)))
          (#_setText down-rate
                     (format nil "D: ~10@a" (format-bytes v-down-rate)))
          (#_setValue max-up (floor v-max-up 1024))
          (#_setValue max-down (floor v-max-down 1024))))))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (setf (torrents window) (list-torrents-humanly)
        (timer window) (#_new QTimer window))
  (let ((vbox (#_new QVBoxLayout window))
        (list (make-instance 'torrent-list
                             :row-key #'cdr
                             :items (torrents window)
                             :selection-behavior :rows))
        (search (#_new QLineEdit))
        (status-bar (add-status-bar window)))
    (setf (torrent-list window) list)
    (add-widgets vbox search list status-bar)
    (connect search "textEdited(QString)" window "search(QString)")))

(defun search-torrent (window text)
  (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) text))
        (torrents (torrents window)))
    (setf (items (torrent-list window)
                 :row-key #'cdr)
          (if (equal "" text)
              torrents
              (remove-if-not
               (lambda (string)
                 (search text string :test #'char-equal))
               torrents
               :key #'second)))))

(defmethod display-menu ((widget torrent-list) item)
  (let ((menu (#_new QMenu)))
    (#_addAction menu "Stop" widget (QSLOT "stopTorrent()"))
    (#_addAction menu "Start" widget (QSLOT "startTorrent()"))
    (#_addAction menu "Remove torrent" widget (QSLOT "removeTorrent()"))
    menu))

(defun stop-torrent (torrent-list)
  (loop for (hash) in (selected-items torrent-list)
        do (stop hash)))

(defun start-torrent (torrent-list)
  (loop for (hash) in (selected-items torrent-list)
        do (start hash)))

(defun remove-torrent (torrent-list)
  (loop for (hash) in (selected-items torrent-list)
        do (erase hash)))

(defun change-max-up (window value)
  (declare (ignore window))
  (set-upload-rate (* value 1024)))

(defun change-max-down (window value)
  (declare (ignore window))
  (set-download-rate (* value 1024)))

;;;

(defmethod view-item ((list torrent-list) item)
  (#_exec (make-instance 'details :torrent (car item))))

(defclass file-list (list-widget)
  ()
  (:metaclass qt-class)
  (:qt-superclass "QTreeView")
  (:slots)
  (:default-initargs
   :header '("Name" "Size" "Priority")))

(defclass details ()
  ((torrent :initarg :torrent
            :initform nil
            :accessor torrent)
   (files :initarg :files
          :initform nil
          :accessor files)
   (file-list :initform nil
              :accessor file-list))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots ("search(QString)" search-file)))

(defun list-files-with-bytes (torrent)
  (process-lists
   '(1 format-bytes
     2 priority-description)
   (list-files-with-size torrent)))

(defmethod initialize-instance :after ((window details) &key torrent)
  (new window)
  (setf (files window) (list-files-with-bytes torrent))
  (let ((vbox (#_new QVBoxLayout window))
        (list (make-instance 'file-list
                             :items (files window)))
        (search (#_new QLineEdit)))
    (setf (file-list window) list)
    (add-widgets vbox search list)
    (connect search "textEdited(QString)"
             window "search(QString)")))

(defun search-file (window text)
  (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) text))
        (files (files window)))
    (setf (items (file-list window))
          (if (equal "" text)
              files
              (remove-if-not
               (lambda (string)
                 (search text string :test #'char-equal))
               files
               :key #'car)))))
