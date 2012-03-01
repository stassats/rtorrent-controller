;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:rtorrent-controller)
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
      (#_hide window))))

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
                  :accessor status-fields))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots ("search(QString)" search-torrent)
          ("statusUpdate()" status-update)))

(defun list-torrents-with-bytes ()
  (loop for (torrent name left state . rest) in (list-torrents)
        collect (list* torrent
                       name
                       (format-bytes left)
                       (if (= state 0)
                           "Stopped"
                           "Started")
                       rest)))

(defun add-permanent-widgets (status-bar &rest widgets)
  (loop for widget in widgets
        do (#_addPermanentWidget status-bar widget)))

(defun add-status-bar (window)
  (let* ((status-bar (#_new QStatusBar))
         (status (status))
         (up-rate (#_new QLabel (format nil "U: ~a" (first status))))
         (down-rate (#_new QLabel (format nil "D: ~a" (second status))))
         (timer (#_new QTimer window)))
    (setf (status-fields window)
          (list :up-rate up-rate
                :down-rate down-rate))
    (add-permanent-widgets status-bar down-rate up-rate)
    
    (connect timer "timeout()" window (qslot "statusUpdate()"))
    (#_start timer 1000)
    status-bar))

(defun status-update (window)
  (let ((fields (status-fields window))
        (status (status)))
    (setf (#_text (getf fields :up-rate))
          (format nil "U: ~a" (first status)))
    (setf (#_text (getf fields :down-rate))
          (format nil "U: ~a" (second status)))))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (setf (torrents window) (list-torrents-with-bytes))
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

;;;

(defmethod view-item ((list torrent-list) item)
  (#_exec (make-instance 'details :torrent (car item))))

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
  (loop for (path size) in (list-files-with-size torrent)
        collect (list path (format-bytes size))))

(defmethod initialize-instance :after ((window details) &key torrent)
  (new window)
  (setf (files window) (list-files-with-bytes torrent))
  (let ((vbox (#_new QVBoxLayout window))
        (list (make-instance 'list-widget
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
