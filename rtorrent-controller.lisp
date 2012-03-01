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

(defun list-files-with-size (torrent)
  (call-rtorrent "f.multicall" torrent
                 "" "f.get_path=" "f.get_size_bytes="))

(defun list-torrents ()
  (call-rtorrent "d.multicall" ""
                 "d.get_hash="
                 "d.get_name="
                 "d.get_left_bytes="
                 "d.get_state="))

(defun stop (hash)
  (call-rtorrent "d.stop" hash))

(defun start (hash)
  (call-rtorrent "d.start" hash))

(defvar *useful-files* '("flac" "cue" "mp3" "ape" "wv" "m4a" "ac3"
                         "pdf" "djvu"
                         "avi" "mkv" "mp4" "vob" "ifo" "bup" "mov"))

(defun useful-file-p (filename)
  (member (pathname-type filename) *useful-files*
          :test #'string-equal))

(defun load-torrent (filename &key start)
  (call-rtorrent (if start
                     "load_start"
                     "load") filename))

(defun disable-not-needed-files (torrent)
  (loop for index from 0
        for file in (list-files torrent)
        unless (useful-file-p file)
        do (call-rtorrent "f.set_priority" torrent index 0)))

(defun disable-last-torrent (&optional (n 1))
    (disable-not-needed-files
     (car (last (call-rtorrent "download_list" "") n))))

(defun process-torrent (filename)
  (when (equal (pathname-type filename)
               "torrent")
    (let ((namestring (remove #\\ (namestring filename))))
      (format t "Loading ~a~%" namestring)
      (handler-case
          (progn
            (load-torrent namestring :start (equal "v" (pathname-name filename)))
            (disable-last-torrent)
            (handler-case (delete-file filename)
              (file-error ())))
        (error (e)
          (warn "An error ~a has occured while communicating with rTorrent." e))))))

(defun load-existing-torrents (directory)
  (mapcar #'process-torrent
	  (directory (merge-pathnames "*.torrent" directory))))

(defun inotify-loop (&optional (directory (user-homedir-pathname)))
  (load-existing-torrents directory)
  (inotify:with-inotify (inot `((,directory ,(logior inotify:in-create
                                                     inotify:in-moved-to))))
    (write-line "Waiting for files.")
    (loop
     (dolist (event (inotify:read-events inot))
       (process-torrent (inotify:event-full-name event))))))
