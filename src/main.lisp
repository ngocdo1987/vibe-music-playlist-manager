;;;; main.lisp - Application entry point for Music Manager
;;;; Server initialization and static file serving

(in-package :music-manager)

;;; Server instance
(defvar *server* nil
        "The Hunchentoot server instance")

(defvar *static-dispatcher* nil
        "Dispatcher for static files")

(defvar *mp3-dispatcher* nil
        "Dispatcher for MP3 files")

(defun get-project-root ()
  "Get the project root directory."
  (asdf:system-source-directory :music-manager))

(defun setup-static-dispatchers ()
  "Set up dispatchers for static files and MP3s."
  (let ((static-dir (merge-pathnames "static/" (get-project-root)))
        (mp3-dir (merge-pathnames "mp3/" (get-project-root))))

    ;; Ensure directories exist
    (ensure-directories-exist static-dir)
    (ensure-directories-exist mp3-dir)

    ;; Create dispatchers
    (setf *static-dispatcher*
      (hunchentoot:create-folder-dispatcher-and-handler
       "/static/" static-dir))

    (setf *mp3-dispatcher*
      (hunchentoot:create-folder-dispatcher-and-handler
       "/mp3/" mp3-dir))

    ;; Add to dispatch table
    (push *static-dispatcher* hunchentoot:*dispatch-table*)
    (push *mp3-dispatcher* hunchentoot:*dispatch-table*)))

(defun configure-hunchentoot ()
  "Configure Hunchentoot settings."
  ;; Set session secret from config
  (setf hunchentoot:*session-secret*
    (get-config "SESSION_SECRET" "default-secret"))

  ;; Set session timeout (1 hour)
  (setf hunchentoot:*session-max-time* 3600)

  ;; Enable debug mode in development
  (setf hunchentoot:*catch-errors-p* t)
  (setf hunchentoot:*show-lisp-errors-p* nil))

(defun start-server (&key (port nil) (host nil))
  "Start the Music Manager web server."
  (when *server*
        (format t "Server already running. Stop it first with (stop-server)~%")
        (return-from start-server nil))

  ;; Load configuration
  (load-config)

  ;; Get port and host from config or arguments
  (let ((server-port (or port (get-config-int "SERVER_PORT" 8080)))
        (server-host (or host (get-config "SERVER_HOST" "0.0.0.0"))))

    ;; Configure Hunchentoot
    (configure-hunchentoot)

    ;; Initialize database
    (init-db)

    ;; Initialize templates
    (init-templates)

    ;; Set up static file serving
    (setup-static-dispatchers)

    ;; Create and start server
    (setf *server*
      (make-instance 'easy-routes:easy-routes-acceptor
        :port server-port
        :address server-host))

    (hunchentoot:start *server*)

    (format t "~%========================================~%")
    (format t "  Music Manager started!~%")
    (format t "  http://~A:~A~%" server-host server-port)
    (format t "  Admin: http://~A:~A/admin~%" server-host server-port)
    (format t "========================================~%~%")

    *server*))

(defun stop-server ()
  "Stop the Music Manager web server."
  (when *server*
        (hunchentoot:stop *server*)
        (setf *server* nil)
        (close-connection)
        (format t "Server stopped.~%")
        t))

(defun restart-server ()
  "Restart the web server."
  (stop-server)
  (reset-config)
  (start-server))

;;; Convenience function for development
(defun dev ()
  "Start server in development mode."
  (start-server :port 8080))
