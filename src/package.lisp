;;;; package.lisp - Package definition for Music Manager
;;;; Defines the namespace and imports for the application

(defpackage :music-manager
  (:use :cl)
  (:import-from :hunchentoot
                :*request*
                :*session*
                :session-value
                :start
                :stop
                :define-easy-handler
                :redirect
                :post-parameter
                :get-parameter
                :script-name
                :request-method
                :content-type
                :raw-post-data)
  (:import-from :easy-routes
                :defroute)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*)
  (:export
   ;; Main entry points
   :start-server
   :stop-server
   
   ;; Configuration
   :load-config
   :get-config
   
   ;; Database
   :init-db
   :get-connection
   
   ;; Models
   :get-all-playlists
   :get-playlist
   :create-playlist
   :update-playlist
   :delete-playlist
   :get-songs-for-playlist
   :add-song-to-playlist
   :remove-song-from-playlist
   :update-song-order
   :create-song
   
   ;; Authentication
   :authenticate
   :login-user
   :logout-user
   :require-auth
   :logged-in-p))
