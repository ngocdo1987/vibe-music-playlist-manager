;;;; models.lisp - Data models for Music Manager
;;;; CRUD operations for playlists and songs

(in-package :music-manager)

;;; ============================================================
;;; Playlist Operations
;;; ============================================================

(defun get-all-playlists ()
  "Get all playlists ordered by creation date."
  (fetch-all
   (execute-sql "SELECT id, name, created_at FROM playlists ORDER BY created_at DESC")))

(defun get-playlist (id)
  "Get a single playlist by ID."
  (fetch-one
   (execute-sql "SELECT id, name, created_at FROM playlists WHERE id = ?" id)))

(defun create-playlist (name)
  "Create a new playlist. Returns the new playlist ID."
  (execute-sql "INSERT INTO playlists (name) VALUES (?)" name)
  (last-insert-id))

(defun update-playlist (id name)
  "Update a playlist's name."
  (execute-sql "UPDATE playlists SET name = ? WHERE id = ?" name id))

(defun delete-playlist (id)
  "Delete a playlist and all its song associations."
  (with-transaction
   ;; Delete associations first (though CASCADE should handle this)
   (execute-sql "DELETE FROM playlist_songs WHERE playlist_id = ?" id)
   ;; Delete the playlist
   (execute-sql "DELETE FROM playlists WHERE id = ?" id)))

;;; ============================================================
;;; Song Operations
;;; ============================================================

(defun get-song (id)
  "Get a single song by ID."
  (fetch-one
   (execute-sql "SELECT id, filename, original_name, created_at FROM songs WHERE id = ?" id)))

(defun get-song-by-filename (filename)
  "Get a song by its filename."
  (fetch-one
   (execute-sql "SELECT id, filename, original_name, created_at FROM songs WHERE filename = ?"
                filename)))

(defun create-song (filename original-name)
  "Create a new song record. Returns the new song ID."
  (execute-sql "INSERT INTO songs (filename, original_name) VALUES (?, ?)"
               filename original-name)
  (last-insert-id))

(defun delete-song (id)
  "Delete a song and all its playlist associations."
  (with-transaction
   (execute-sql "DELETE FROM playlist_songs WHERE song_id = ?" id)
   (execute-sql "DELETE FROM songs WHERE id = ?" id)))

(defun get-all-songs ()
  "Get all songs ordered by original name."
  (fetch-all
   (execute-sql "SELECT id, filename, original_name, created_at FROM songs ORDER BY original_name")))

;;; ============================================================
;;; Playlist-Song Relationship Operations
;;; ============================================================

(defun get-songs-for-playlist (playlist-id)
  "Get all songs in a playlist, ordered by position."
  (fetch-all
   (execute-sql
    "SELECT s.id, s.filename, s.original_name, s.created_at, ps.position
     FROM songs s
     JOIN playlist_songs ps ON s.id = ps.song_id
     WHERE ps.playlist_id = ?
     ORDER BY ps.position"
    playlist-id)))

(defun add-song-to-playlist (playlist-id song-id &optional position)
  "Add a song to a playlist at the specified position.
   If position is nil, adds at the end."
  (let ((pos (or position
                 (1+ (or (getf (fetch-one
                                (execute-sql
                                 "SELECT MAX(position) as max_pos FROM playlist_songs WHERE playlist_id = ?"
                                 playlist-id))
                               :|max_pos|)
                         0)))))
    (execute-sql "INSERT OR REPLACE INTO playlist_songs (playlist_id, song_id, position) VALUES (?, ?, ?)"
                 playlist-id song-id pos)))

(defun remove-song-from-playlist (playlist-id song-id)
  "Remove a song from a playlist."
  (execute-sql "DELETE FROM playlist_songs WHERE playlist_id = ? AND song_id = ?"
               playlist-id song-id))

(defun update-song-order (playlist-id song-ids)
  "Update the order of songs in a playlist.
   song-ids is a list of song IDs in the desired order."
  (with-transaction
   ;; Delete all current associations
   (execute-sql "DELETE FROM playlist_songs WHERE playlist_id = ?" playlist-id)
   ;; Re-insert with new positions
   (loop for song-id in song-ids
         for position from 1
         do (execute-sql
             "INSERT INTO playlist_songs (playlist_id, song_id, position) VALUES (?, ?, ?)"
             playlist-id song-id position))))

(defun get-playlist-count ()
  "Get the total number of playlists."
  (getf (fetch-one (execute-sql "SELECT COUNT(*) as count FROM playlists")) :|count|))

(defun get-song-count ()
  "Get the total number of songs."
  (getf (fetch-one (execute-sql "SELECT COUNT(*) as count FROM songs")) :|count|))

(defun playlist-exists-p (id)
  "Check if a playlist exists."
  (not (null (get-playlist id))))

(defun song-in-playlist-p (playlist-id song-id)
  "Check if a song is in a playlist."
  (fetch-one
   (execute-sql "SELECT 1 FROM playlist_songs WHERE playlist_id = ? AND song_id = ?"
                playlist-id song-id)))
