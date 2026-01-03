;;;; music-manager.asd - ASDF system definition for Music Manager
;;;; A web application to manage MP3 playlists

(defsystem "music-manager"
  :version "1.0.0"
  :author "Ngoc"
  :license "MIT"
  :description "A Lisp web application to manage MP3 songs and playlists"
  
  ;; External dependencies from Quicklisp
  :depends-on ("hunchentoot"      ; Web server
               "easy-routes"      ; URL routing
               "djula"            ; HTML templating
               "cl-dbi"           ; Database interface
               "dbd-sqlite3"      ; SQLite driver
               "cl-dotenv"        ; Environment variable loading
               "cl-json"          ; JSON encoding/decoding
               "alexandria"       ; Common utilities
               "uiop")            ; File system utilities
  
  ;; Source files in load order
  :components ((:module "src"
                :components ((:file "package")
                             (:file "config")
                             (:file "db")
                             (:file "models")
                             (:file "auth")
                             (:file "upload")
                             (:file "routes")
                             (:file "main"))))
  
  ;; Test system
  :in-order-to ((test-op (test-op "music-manager/tests"))))

;; Test system definition
(defsystem "music-manager/tests"
  :depends-on ("music-manager" "fiveam")
  :components ((:module "tests"
                :components ((:file "main-test"))))
  :perform (test-op (o s)
             (uiop:symbol-call :fiveam :run! 
               (uiop:find-symbol* :music-manager-tests :music-manager/tests))))
