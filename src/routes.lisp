;;;; routes.lisp - HTTP routes for Music Manager
;;;; All endpoint definitions for public and admin areas

(in-package :music-manager)

;;; ============================================================
;;; Template Rendering Helpers
;;; ============================================================

(defvar *template-path* nil
        "Path to templates directory")

(defun init-templates ()
  "Initialize Djula template directories."
  (let ((base-path (merge-pathnames "templates/" (asdf:system-source-directory :music-manager))))
    (setf *template-path* base-path)
    ;; Add subdirectories first for child templates, then root for any root templates
    (djula:add-template-directory (merge-pathnames "admin/" base-path))
    (djula:add-template-directory (merge-pathnames "public/" base-path))
    (djula:add-template-directory base-path)))

(defun render-template (template-name &rest args)
  "Render a Djula template with the given arguments."
  (let ((template (djula:compile-template* template-name)))
    (apply #'djula:render-template* template nil args)))

(defun render-admin-template (template-name &rest args)
  "Render an admin template with common variables."
  (let ((flash (get-flash-message)))
    (apply #'render-template template-name
      :user (current-user)
      :flash-message (car flash)
      :flash-type (cdr flash)
      args)))

;;; ============================================================
;;; Public Routes
;;; ============================================================

(easy-routes:defroute home-route ("/" :method :get) ()
                      "Home page - redirect to first playlist or show welcome."
                      (let ((playlists (get-all-playlists)))
                        (if playlists
                            (hunchentoot:redirect (format nil "/playlist/~A" (getf (first playlists) :|id|)))
                            (render-template "welcome.html"))))

(easy-routes:defroute playlist-view-route ("/playlist/:id" :method :get) (&path (id 'integer))
                      "Public playlist view with audio player."
                      (let ((playlist (get-playlist id)))
                        (if playlist
                            (let ((songs (get-songs-for-playlist id)))
                              (render-template "playlist.html"
                                               :playlist playlist
                                               :songs songs))
                            (progn
                             (setf (hunchentoot:return-code*) 404)
                             (render-template "404.html")))))

;;; ============================================================
;;; Authentication Routes
;;; ============================================================

(easy-routes:defroute login-page-route ("/admin/login" :method :get) ()
                      "Show login form."
                      (if (logged-in-p)
                          (hunchentoot:redirect "/admin")
                          (let ((flash (get-flash-message)))
                            (render-template "login.html"
                                             :flash-message (car flash)
                                             :flash-type (cdr flash)))))

(easy-routes:defroute login-submit-route ("/admin/login" :method :post) ()
                      "Process login form submission."
                      (let ((username (hunchentoot:post-parameter "username"))
                            (password (hunchentoot:post-parameter "password")))
                        (if (authenticate username password)
                            (progn
                             (login-user username)
                             (hunchentoot:redirect (get-redirect-after-login)))
                            (progn
                             (set-flash-message "Invalid username or password" :error)
                             (hunchentoot:redirect "/admin/login")))))

(easy-routes:defroute logout-route ("/admin/logout" :method :get) ()
                      "Logout and redirect to login page."
                      (logout-user)
                      (set-flash-message "You have been logged out" :info)
                      (hunchentoot:redirect "/admin/login"))

;;; ============================================================
;;; Admin Dashboard Routes
;;; ============================================================

(easy-routes:defroute admin-dashboard-route ("/admin" :method :get) ()
                      "Admin dashboard showing all playlists."
                      (unless (require-auth) (return-from admin-dashboard-route))
                      (let ((playlists (get-all-playlists)))
                        (render-admin-template "dashboard.html"
                                               :playlists playlists
                                               :playlist-count (length playlists)
                                               :song-count (get-song-count))))

;;; ============================================================
;;; Playlist CRUD Routes
;;; ============================================================

(easy-routes:defroute playlist-new-route ("/admin/playlist/new" :method :get) ()
                      "Show new playlist form."
                      (unless (require-auth) (return-from playlist-new-route))
                      (render-admin-template "playlist-form.html"
                                             :action "/admin/playlist"
                                             :method "POST"
                                             :title "Create New Playlist"))

(easy-routes:defroute playlist-create-route ("/admin/playlist" :method :post) ()
                      "Create a new playlist."
                      (unless (require-auth) (return-from playlist-create-route))
                      (let* ((name (hunchentoot:post-parameter "name")))
                        (if (and name (> (length (string-trim '(#\Space) name)) 0))
                            (let ((playlist-id (create-playlist name)))
                              (set-flash-message "Playlist created successfully" :success)
                              (hunchentoot:redirect (format nil "/admin/playlist/~A/edit" playlist-id)))
                            (progn
                             (set-flash-message "Playlist name is required" :error)
                             (hunchentoot:redirect "/admin/playlist/new")))))

(easy-routes:defroute playlist-edit-route ("/admin/playlist/:id/edit" :method :get)
                      (&path (id 'integer))
                      "Show edit playlist form."
                      (unless (require-auth) (return-from playlist-edit-route))
                      (let ((playlist (get-playlist id)))
                        (if playlist
                            (let ((songs (get-songs-for-playlist id)))
                              (render-admin-template "playlist-form.html"
                                                     :action (format nil "/admin/playlist/~A" id)
                                                     :method "POST"
                                                     :title "Edit Playlist"
                                                     :playlist playlist
                                                     :songs songs))
                            (progn
                             (set-flash-message "Playlist not found" :error)
                             (hunchentoot:redirect "/admin")))))

(easy-routes:defroute playlist-update-route ("/admin/playlist/:id" :method :post)
                      (&path (id 'integer))
                      "Update a playlist."
                      (unless (require-auth) (return-from playlist-update-route))
                      (let ((name (hunchentoot:post-parameter "name")))
                        (if (and name (> (length (string-trim '(#\Space) name)) 0))
                            (progn
                             (update-playlist id name)
                             (set-flash-message "Playlist updated successfully" :success))
                            (set-flash-message "Playlist name is required" :error))
                        (hunchentoot:redirect (format nil "/admin/playlist/~A/edit" id))))

(easy-routes:defroute playlist-delete-route ("/admin/playlist/:id/delete" :method :post)
                      (&path (id 'integer))
                      "Delete a playlist."
                      (unless (require-auth) (return-from playlist-delete-route))
                      (let ((playlist (get-playlist id)))
                        (when playlist
                              (delete-playlist id)
                              (set-flash-message "Playlist deleted successfully" :success)))
                      (hunchentoot:redirect "/admin"))

;;; ============================================================
;;; File Upload Routes
;;; ============================================================

(easy-routes:defroute upload-files-route ("/admin/upload" :method :post) ()
                      "Handle MP3 file upload. Returns JSON response."
                      (unless (require-auth)
                        (setf (hunchentoot:content-type*) "application/json")
                        (return-from upload-files-route "{\"error\": \"Unauthorized\"}"))

                      (setf (hunchentoot:content-type*) "application/json")

                      (let* ((playlist-id (parse-integer (or (hunchentoot:post-parameter "playlist_id") "")
                                                         :junk-allowed t))
                             (files (hunchentoot:post-parameters*))
                             (uploaded-songs '()))

                        ;; Process each uploaded file
                        (loop for (name . value) in files
                                when (and (string= name "files") (listp value))
                              do (let ((temp-path (first value))
                                       (original-name (second value)))
                                   (multiple-value-bind (success result)
                                       (save-uploaded-file temp-path original-name)
                                     (if success
                                         (let ((song-id (create-song result original-name)))
                                           (when playlist-id
                                                 (add-song-to-playlist playlist-id song-id))
                                           (push (list :id song-id
                                                       :filename result
                                                       :original_name original-name
                                                       :url (get-mp3-url result))
                                                 uploaded-songs))
                                         (push (list :error result :filename original-name)
                                               uploaded-songs)))))

                        (cl-json:encode-json-to-string
                          (list :success t :songs (nreverse uploaded-songs)))))

(easy-routes:defroute update-song-order-route ("/admin/playlist/:id/order" :method :post)
                      (&path (id 'integer))
                      "Update song order in a playlist. Expects JSON body with song_ids array."
                      (unless (require-auth)
                        (setf (hunchentoot:content-type*) "application/json")
                        (return-from update-song-order-route "{\"error\": \"Unauthorized\"}"))

                      (setf (hunchentoot:content-type*) "application/json")

                      (handler-case
                          (let* ((body (hunchentoot:raw-post-data :force-text t))
                                 (data (cl-json:decode-json-from-string body))
                                 (song-ids (cdr (assoc :song--ids data))))
                            (when song-ids
                                  (update-song-order id song-ids))
                            (cl-json:encode-json-to-string (list :success t)))
                        (error (e)
                          (cl-json:encode-json-to-string
                            (list :success nil :error (format nil "~A" e))))))

(easy-routes:defroute remove-song-route ("/admin/playlist/:playlist-id/song/:song-id/remove"
                                         :method :post)
                      (&path (playlist-id 'integer) (song-id 'integer))
                      "Remove a song from a playlist."
                      (unless (require-auth)
                        (setf (hunchentoot:content-type*) "application/json")
                        (return-from remove-song-route "{\"error\": \"Unauthorized\"}"))

                      (setf (hunchentoot:content-type*) "application/json")
                      (remove-song-from-playlist playlist-id song-id)
                      (cl-json:encode-json-to-string (list :success t)))
