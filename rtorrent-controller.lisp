;;; -*- Mode: Lisp -*-

;;; This software is in the public domain and is
;;; provided with absolutely no warranty.

(in-package #:rtorrent-controller)

(defvar *host* "localhost")

(defun call-rtorrent (method &rest arguments)
  (s-xml-rpc:xml-rpc-call (apply #'s-xml-rpc:encode-xml-rpc-call
                                 method arguments)
                          :host *host*))

(defun list-files (torrent)
  (mapcar #'car
          (call-rtorrent "f.multicall" torrent
                         "" "f.get_path=")))

(defun list-files-with-size (torrent)
  (call-rtorrent "f.multicall" torrent
                 "" "f.get_path="
                 "f.get_size_bytes="
                 "f.get_priority=" ))

(defun list-torrents ()
  (call-rtorrent "d.multicall" ""
                 "d.get_hash="
                 "d.get_name="
                 "d.get_left_bytes="
                 "d.get_state="
                 "d.get_down_rate="))

(defun enabled-files-finished-p (torrent)
  (loop for (priority chunks completed-chunks) in
        (call-rtorrent "f.multicall" torrent ""
                       "f.get_priority="
                       "f.get_size_chunks="
                       "f.get_completed_chunks=")
        always (or (zerop priority)
                   (= chunks completed-chunks))))

(defun list-finished-torrents ()
  (let ((torrents (call-rtorrent "d.multicall" ""
                                 "d.get_hash="
                                 "d.get_base_path="
                                 "d.get_left_bytes=")))
    (loop for (hash path size-left) in torrents
          when (and (not (equal path ""))
                    (or (zerop size-left)
                        (enabled-files-finished-p hash)))
          collect path)))

(defun stop (hash)
  (call-rtorrent "d.stop" hash))

(defun start (hash)
  (call-rtorrent "d.start" hash))

(defun erase (hash)
  (call-rtorrent "d.erase" hash))

(defun multicall (&rest args)
  (call-rtorrent "system.multicall"
                 (loop for (method . parameters) in args
                       collect
                       (s-xml-rpc:xml-rpc-struct "methodName" method "params"
                                                 (or parameters
                                                     #())))))
(defun format-bytes (bytes)
  (loop for unit in '("" "KB" "MB" "GB" "TB")
        for prev = 1 then multiplier
        for multiplier = 1024 then (* multiplier 1024)
        when (< bytes multiplier)
        return (format nil "~$ ~a" (/ bytes prev) unit)))

(defun process-list (functions list)
  (loop for x in list
        for i from 0
        collect (funcall (getf functions i #'identity)
                         x)))

(defun process-lists (functions lists)
  (loop for list in lists
        collect (process-list functions list)))

(defun state-description (n)
  (ecase n
    (0 "Stopped")
    (1 "Started")))

(defun priority-description (n)
  (ecase n
    (0 "Off")
    (1 "Normal")
    (2 "High")))

(defun status ()
  (mapcar #'car (multicall '("get_up_rate") '("get_down_rate")
                           '("get_upload_rate") '("get_download_rate"))))

(defun set-download-rate (value)
  (call-rtorrent "set_download_rate" value))

(defun set-upload-rate (value)
  (call-rtorrent "set_upload_rate" value))

;;;

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

(defun run-program (program args)
  #+sbcl
  (sb-ext:run-program program args
                      :search t)
  #+ccl
  (ccl:run-program program args)
  #-(or sbcl ccl)
  (error "Not supported"))

(defun remove-file (filename)
  (handler-case (delete-file filename)
    (file-error ())))

(defun native-namestring (file)
  #+sbcl (sb-ext:native-namestring file)
  #+ccl (ccl:native-translated-namestring file)
  #-(or sbcl ccl) (error "Not supported"))

(defun move-file (filename)
  (run-program "rsync" (list "-az"
                             "--delay-updates"
                             (sb-ext:native-namestring filename)
                             "desktop:/tmp/"))
  (remove-file filename))

(defun process-torrent (filename)
  (format t "Loading ~a~%" filename)
  (if (equal (machine-instance) "laptop")
      (move-file filename)
      (let ((namestring (remove #\\ (namestring filename))))
        (load-torrent namestring :start (equal "v" (pathname-name filename)))
        (disable-last-torrent)
        (remove-file filename))))

(defun process-subtitle (filename)
  (format t "Loading ~a~%" filename)
  (cond ((equal (machine-instance) "laptop")
         (move-file filename))
        (t
         (let ((new-path (namestring (merge-pathnames
                                      (make-pathname
                                       :directory '(:relative "sub")
                                       :defaults filename)
                                      (user-homedir-pathname)))))
          (run-program "mv"
                       (list (namestring  filename) new-path)) 
          (run-program "rj" (list new-path))))))

(defun process-zip-subtitle (filename)
  (format t "Loading ~a~%" filename)
  (cond ((equal (machine-instance) "laptop")
         (move-file filename))
        (t
         (run-program "us"
                      (list
                       (namestring filename))))))

(defun load-existing-torrents (directory)
  (mapcar #'process-torrent
	  (directory (merge-pathnames "*.torrent" directory))))

(defun inotify-loop (&key (directory #p"/tmp/"))
  (load-existing-torrents directory)
  (inotify:with-inotify (inot `((,directory ,inotify:in-moved-to)))
    (write-line "Waiting for files.")
    (loop
     (dolist (event (inotify:read-events inot))
       (handler-case
         (let* ((path (inotify:event-full-name event))
                (type (pathname-type path)))
           (cond ((equal type "torrent")
                  (process-torrent path))
                 ((and (equal type "zip")
                       (equal (pathname-name path) "g"))
                  (process-zip-subtitle path))
                 ((equal type "srt")
                  (process-subtitle path))))
         (error (e)
           (warn "An error ~a occured while processing event ~s"
                 e event)))))))

#+(or)
(ccl:save-application "rtr-controller"
                      :toplevel-function #'rtorrent-controller:inotify-loop
                      :prepend-kernel t)
#+(or)
(sb-ext:save-lisp-and-die "rtr-controller"
                          :toplevel #'rtorrent-controller:inotify-loop
                          :executable t
                          :compression 9)
