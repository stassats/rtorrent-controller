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
  (:slots)
  (:default-initargs
   :header '("Hash" "Name" "Left to download")))

(defclass main-window ()
  ((torrents :initform nil
             :accessor torrents)
   (torrent-list :initform nil
                :accessor torrent-list))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots ("search(QString)" search-torrent)))

(defun format-bytes (bytes)
  (loop for unit in '("" "KB" "MB" "GB" "TB")
        for prev = 1 then multiplier
        for multiplier = 1024 then (* multiplier 1024)
        when (< bytes multiplier)
        return (format nil "~$ ~a" (/ bytes prev) unit)))

(defun list-torrents-with-bytes ()
  (loop for (torrent name left) in (list-torrents)
        collect (list torrent name (format-bytes left))))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (setf (torrents window) (list-torrents-with-bytes))
  (let ((vbox (#_new QVBoxLayout window))
        (list (make-instance 'torrent-list
                             :items (torrents window)))
        (search (#_new QLineEdit)))
    (setf (torrent-list window) list)
    (add-widgets vbox search list)
    (connect search "textEdited(QString)"
             window "search(QString)")))

(defun search-torrent (window text)
  (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) text))
        (torrents (torrents window)))
    (setf (items (torrent-list window))
          (if (equal "" text)
              torrents
              (remove-if-not
               (lambda (string)
                 (search text string :test #'char-equal))
               torrents
               :key #'second)))))

(defmethod view-item ((list torrent-list) item)
  (#_exec (make-instance 'details :torrent item)))

(defclass details ()
  ((torrent :initarg :torrent
            :initform nil
            :accessor torrent)
   (files :initform nil
          :accessor files))
  (:metaclass qt-class)
  (:qt-superclass "QDialog")
  (:slots ("search(QString)" search-file)))

(defun list-files-with-bytes (torrent)
  (loop for (path size) in (list-files-with-size torrent)
        collect (list path (format-bytes size))))

(defmethod initialize-instance :after ((window details) &key torrent)
  (new window)

  (let ((vbox (#_new QVBoxLayout window))
        (list (make-instance 'list-widget
                             :items (list-files-with-bytes torrent)))
        (search (#_new QLineEdit)))
    (setf (files window) list)
    (add-widgets vbox search list)
    (connect search "textEdited(QString)"
             window "search(QString)")))

(defun search-file (window text)
  (let ((text (string-trim '(#\Space #\Newline #\Return #\Tab) text))
        (files (items (files window))))
    (setf (items (files window))
          (if (equal "" text)
              files
              (remove-if-not
               (lambda (string)
                 (search text string :test #'char-equal))
               files)))))
