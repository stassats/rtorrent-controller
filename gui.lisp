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
          ("statusUpdate()" status-update)
          ("changeMaxUp(int)" change-max-up)
          ("changeMaxDown(int)" change-max-down)))

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
         (max-up (#_new QSpinBox))
         (max-down (#_new QSpinBox))
         (timer (#_new QTimer window)))
    (setf (status-fields window)
          (list :up-rate (#_new QLabel)
                :max-up max-up
                :down-rate (#_new QLabel)
                :max-down max-down))

    (setf (#_singleStep max-up) 50
          (#_singleStep max-down) 50
          (#_maximum max-up) 10000
          (#_maximum max-down) 10000)

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
          (setf (#_text up-rate)
                (format nil "U: ~10@a" (format-bytes v-up-rate))
                (#_text down-rate)
                (format nil "D: ~10@a" (format-bytes v-down-rate))
                (#_value max-up) (floor v-max-up 1024)
                (#_value max-down) (floor v-max-down 1024))))))

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

(defun change-max-up (window value)
  (declare (ignore window))
  (set-upload-rate (* value 1024)))

(defun change-max-down (window value)
  (declare (ignore window))
  (set-download-rate (* value 1024)))

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
