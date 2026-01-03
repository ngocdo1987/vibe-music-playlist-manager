;;;; config.lisp - Configuration module for Music Manager
;;;; Loads environment variables from .env file

(in-package :music-manager)

;;; Configuration storage
(defvar *config* (make-hash-table :test 'equal)
  "Hash table storing configuration values")

(defvar *config-loaded* nil
  "Flag indicating if configuration has been loaded")

;;; Required configuration keys
(defparameter *required-keys* 
  '("ADMIN_USER" "ADMIN_PASS")
  "List of required configuration keys that must be present")

;;; Default configuration values
(defparameter *default-config*
  '(("SERVER_PORT" . "8080")
    ("SERVER_HOST" . "0.0.0.0")
    ("DB_PATH" . "music-manager.db")
    ("SESSION_SECRET" . "default-secret-change-me"))
  "Default values for optional configuration keys")

(defun find-env-file ()
  "Find the .env file, checking current directory and project root."
  (let ((paths (list ".env"
                     (merge-pathnames ".env" (asdf:system-source-directory :music-manager)))))
    (loop for path in paths
          when (probe-file path)
            return (probe-file path))))

(defun parse-env-line (line)
  "Parse a single line from .env file. Returns (key . value) or nil."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
    ;; Skip empty lines and comments
    (when (and (> (length trimmed) 0)
               (char/= (char trimmed 0) #\#))
      (let ((pos (position #\= trimmed)))
        (when pos
          (cons (string-trim '(#\Space #\Tab) (subseq trimmed 0 pos))
                (string-trim '(#\Space #\Tab #\") 
                             (subseq trimmed (1+ pos)))))))))

(defun load-env-file (path)
  "Load environment variables from a file into *config*."
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (when stream
      (loop for line = (read-line stream nil nil)
            while line
            for parsed = (parse-env-line line)
            when parsed
              do (setf (gethash (car parsed) *config*) (cdr parsed))))))

(defun apply-defaults ()
  "Apply default values for any missing optional configuration."
  (loop for (key . value) in *default-config*
        unless (gethash key *config*)
          do (setf (gethash key *config*) value)))

(defun validate-config ()
  "Validate that all required configuration keys are present."
  (let ((missing (loop for key in *required-keys*
                       unless (gethash key *config*)
                         collect key)))
    (when missing
      (error "Missing required configuration keys: ~{~A~^, ~}~%~
              Please create a .env file with these values." missing))))

(defun load-config ()
  "Load configuration from .env file. Must be called before using get-config."
  (unless *config-loaded*
    ;; Clear existing config
    (clrhash *config*)
    
    ;; Find and load .env file
    (let ((env-path (find-env-file)))
      (if env-path
          (progn
            (format t "Loading configuration from: ~A~%" env-path)
            (load-env-file env-path))
          (format t "Warning: No .env file found, using defaults only~%")))
    
    ;; Apply defaults for missing optional values
    (apply-defaults)
    
    ;; Validate required keys
    (validate-config)
    
    (setf *config-loaded* t)
    (format t "Configuration loaded successfully~%"))
  *config*)

(defun get-config (key &optional default)
  "Get a configuration value by key. Returns default if not found."
  (unless *config-loaded*
    (load-config))
  (or (gethash key *config*) default))

(defun get-config-int (key &optional (default 0))
  "Get a configuration value as an integer."
  (parse-integer (get-config key (write-to-string default))))

(defun reset-config ()
  "Reset configuration state, forcing reload on next access."
  (clrhash *config*)
  (setf *config-loaded* nil))
