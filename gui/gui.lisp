;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:rtorrent-gui)
(named-readtables:in-readtable :qt)

(defvar *qapp* nil)

(defclass torrent ()
  ((hash :initarg :hash
         :initform nil
         :accessor hash)
   (name :initarg :name
         :initform nil
         :accessor name) 
   (imdb-id :initarg :imdb-id
            :initform nil
            :accessor imdb-id)
   (tracking-object :initarg :tracking-object
                    :initform nil
                    :accessor tracking-object)
   (completed :initarg :completed
              :initform nil
              :accessor completed)))

(defmethod object-description ((torrent torrent) &key)
  (with-slots (name tracking-object) torrent
    (if tracking-object
        (object-description tracking-object)
        name)))

(defvar *torrents* nil)
(defvar *torrents-to-tracking*
  '(("A24ECB836E1AC4AEFF9E0A758A450F8320187A5F" . 53946)))

(defun gui ()
  (unless *qapp*
    (setf *qapp* (make-qapplication)))
  (let ((window (make-instance 'main-window)))
    (unwind-protect
         (progn
           (#_show window)
           (#_exec *qapp*))
      (when (timer window)
        (#_stop (timer window)))
      (#_hide window))))

(defun find-tracking-object (imdb-id)
  (tracking:with-tracking
    (storage:lookup 'tracking:movie
                    (storage:where 'tracking:imdb-id imdb-id))))

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
  (:qt-superclass "QMainWindow")
  (:slots ("search(QString)" search-torrent)
          ("statusUpdate()" status-update)
          ("changeMaxUp(int)" change-max-up)
          ("changeMaxDown(int)" change-max-down)))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (setf (torrents window) (list-torrent-objects)
        (timer window) (#_new QTimer window))
  ;(make-torrent-list (torrents window))
  (let* ((central-widget (#_new QWidget window))
         (vbox (#_new QVBoxLayout central-widget))
         (list (make-instance 'torrent-list
                              :items (torrents window)
                              :selection-behavior :rows))
         (search (#_new QLineEdit))
         (status-bar (add-status-bar window))
         (tab-widget (#_new QTabWidget)))
    (#_setCentralWidget window central-widget)
    (setf (torrent-list window) list)
    (add-widgets vbox search tab-widget status-bar)
    (#_addTab tab-widget list "All")
    (connect search "textEdited(QString)" window "search(QString)")))

(defun ensure-torrent (hash name)
  (or (find hash *torrents* :key #'hash :test #'equal)
      (car (push (make-instance 'torrent :hash hash :name name)
                 *torrents*))))

(defun list-torrents-humanly ()
  (process-lists
   '(2 format-bytes
     3 state-description
     4 format-bytes)
   (list-torrents)))

(defun list-torrent-objects ()
  (loop for (hash name . rest) in (list-torrents-humanly)
        collect (list* (ensure-torrent hash name) rest)))

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

(defun search-torrent (window text)
  (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) text))
        (torrents (torrents window)))
    (setf (items (torrent-list window))
          (if (equal "" text)
              torrents
              (remove-if-not
               (lambda (torrent)
                 (search text (object-description torrent) :test #'char-equal))
               torrents
               :key #'car)))))

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
  (loop for (torrent) in (selected-items torrent-list)
        do (erase (hash torrent))))

(defun change-max-up (window value)
  (declare (ignore window))
  (set-upload-rate (* value 1024)))

(defun change-max-down (window value)
  (declare (ignore window))
  (set-download-rate (* value 1024)))

;;;

(defmethod view-item ((list torrent-list) item)
  (#_show (make-instance 'details :torrent (car item))))

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

(defmethod initialize-instance :after ((window details) &key torrent)
  (new window)
  (setf (files window) (list-files-with-bytes (hash torrent)))
  (let ((vbox (#_new QVBoxLayout window))
        (list (make-instance 'file-list
                             :items (files window)))
        (search (#_new QLineEdit)))
    (setf (file-list window) list)
    (when (tracking-object torrent)
      (tracking-qt:add-link vbox (tracking-object torrent)))
    (add-widgets vbox search list)
    (connect search "textEdited(QString)"
             window "search(QString)")))

(defun list-files-with-bytes (torrent)
  (process-lists
   '(1 format-bytes
     2 priority-description)
   (list-files-with-size torrent)))

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
