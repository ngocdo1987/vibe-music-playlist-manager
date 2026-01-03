;;;; db.lisp - Database module for Music Manager
;;;; Handles SQLite connection and schema initialization

(in-package :music-manager)

;;; Database connection
(defvar *db-connection* nil
        "Current database connection")

(defun get-db-path ()
  "Get the database file path from configuration."
  (let ((db-path (get-config "DB_PATH" "music-manager.db")))
    ;; If relative path, make it relative to project root
    (if (uiop:relative-pathname-p db-path)
        (merge-pathnames db-path (asdf:system-source-directory :music-manager))
        db-path)))

(defun get-connection ()
  "Get the current database connection, creating one if needed."
  (unless *db-connection*
    (setf *db-connection*
      (dbi:connect :sqlite3 :database-name (namestring (get-db-path)))))
  *db-connection*)

(defun close-connection ()
  "Close the database connection if open."
  (when *db-connection*
        (dbi:disconnect *db-connection*)
        (setf *db-connection* nil)))

;;; Schema definitions
(defparameter *schema-sql*
              '("CREATE TABLE IF NOT EXISTS playlists (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       name TEXT NOT NULL,
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
     )"

                "CREATE TABLE IF NOT EXISTS songs (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       filename TEXT NOT NULL UNIQUE,
       original_name TEXT NOT NULL,
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
     )"

                "CREATE TABLE IF NOT EXISTS playlist_songs (
       playlist_id INTEGER NOT NULL,
       song_id INTEGER NOT NULL,
       position INTEGER NOT NULL,
       PRIMARY KEY (playlist_id, song_id),
       FOREIGN KEY (playlist_id) REFERENCES playlists(id) ON DELETE CASCADE,
       FOREIGN KEY (song_id) REFERENCES songs(id) ON DELETE CASCADE
     )"

                "CREATE INDEX IF NOT EXISTS idx_playlist_songs_playlist 
       ON playlist_songs(playlist_id)"

                "CREATE INDEX IF NOT EXISTS idx_playlist_songs_position 
       ON playlist_songs(playlist_id, position)")
              "SQL statements to create database schema")

(defun init-db ()
  "Initialize the database, creating tables if they don't exist."
  (let ((conn (get-connection)))
    (format t "Initializing database at: ~A~%" (get-db-path))

    ;; Enable foreign keys for SQLite
    (dbi:do-sql conn "PRAGMA foreign_keys = ON")

    ;; Create all tables
    (dolist (sql *schema-sql*)
      (dbi:do-sql conn sql))

    (format t "Database initialized successfully~%")
    conn))

;;; Query helpers
(defun execute-sql (sql &rest params)
  "Execute a SQL statement with parameters. Returns the result."
  (let* ((conn (get-connection))
         (query (dbi:prepare conn sql))
         (result (dbi:execute query params)))
    result))

(defun fetch-all (result)
  "Fetch all rows from a query result as a list of plists."
  (dbi:fetch-all result))

(defun fetch-one (result)
  "Fetch a single row from a query result as a plist."
  (dbi:fetch result))

(defun last-insert-id ()
  "Get the last inserted row ID."
  (let* ((result (execute-sql "SELECT last_insert_rowid() as id"))
         (row (fetch-one result)))
    (getf row :|id|)))

;;; Transaction support
(defmacro with-transaction (&body body)
  "Execute body within a database transaction."
  `(let ((conn (get-connection)))
     (dbi:do-sql conn "BEGIN TRANSACTION")
     (handler-case
         (prog1 (progn ,@body)
           (dbi:do-sql conn "COMMIT"))
       (error (e)
         (dbi:do-sql conn "ROLLBACK")
         (error e)))))
