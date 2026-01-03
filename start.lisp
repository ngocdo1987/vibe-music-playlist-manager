;;; start.lisp - Production startup script for Music Manager
;;; This file is used by systemd to start the application

;; Load Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
        (load quicklisp-init)))

;; Load the music-manager system
(ql:quickload :music-manager)

;; Start the server
(music-manager:start-server)

;; Keep the process running
(loop (sleep 86400))
