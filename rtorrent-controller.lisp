;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:rtorrent-controller)

(defun call-rtorrent (method &rest arguments)
  (s-xml-rpc:xml-rpc-call (apply #'s-xml-rpc:encode-xml-rpc-call
                                 method arguments)
                          :host "localhost"))

(defun list-files (torrent)
  (mapcar #'car
          (call-rtorrent "f.multicall" torrent
                         "" "f.get_path=")))

(defun list-torrents ()
  (loop for torrent in (call-rtorrent "download_list" "")
        do (format t "~a ~s~%"
                   torrent
                   (call-rtorrent "d.get_name" torrent))))

(defvar *useful-files* '("flac" "cue" "mp3" "ape" "wv"
                         "pdf" "djvu"
                         "avi" "mkv" "mp4" "vob" "ifo" "bup" "mov"))

(defun useful-file-p (filename)
  (member (pathname-type filename) *useful-files*
          :test #'string-equal))

(defun load-torrent (filename)
  (call-rtorrent "load" filename))

(defun disable-not-needed-files (torrent)
  (loop for index from 0
        for file in (list-files torrent)
        unless (useful-file-p file)
        do (call-rtorrent "f.set_priority" torrent index 0)))

(defun disable-last-torrent (&optional (n 1))
    (disable-not-needed-files
     (car (last (call-rtorrent "download_list" "") n))))

(defun inotify-loop (directory)
  (inotify:with-inotify (inot `((,directory ,inotify:in-create)))
    (loop (let ((filename (inotify:event-full-name
                           (inotify:read-event inot))))
            (when (equal (pathname-type filename)
                         "torrent")
              (format t "Loading ~a~%" filename)
              (load-torrent (namestring filename))
              (disable-last-torrent))))))
