;;;; auth.lisp - Authentication module for Music Manager
;;;; Session-based login/logout using Hunchentoot sessions

(in-package :music-manager)

;;; Session key constants
(defparameter *session-user-key* :logged-in-user
              "Session key for storing the logged-in username")

(defparameter *session-login-time-key* :login-time
              "Session key for storing login timestamp")

;;; Authentication functions

(defun authenticate (username password)
  "Check if the provided credentials match the configured admin credentials.
   Returns T if valid, NIL otherwise."
  (and (stringp username)
       (stringp password)
       (string= username (get-config "ADMIN_USER"))
       (string= password (get-config "ADMIN_PASS"))))

(defun logged-in-p ()
  "Check if the current request has an authenticated session."
  (and hunchentoot:*session*
       (hunchentoot:session-value *session-user-key*)))

(defun current-user ()
  "Get the username of the currently logged-in user, or NIL if not logged in."
  (when hunchentoot:*session*
        (hunchentoot:session-value *session-user-key*)))

(defun login-user (username)
  "Create a session for the authenticated user."
  ;; Start a new session if one doesn't exist
  (hunchentoot:start-session)
  ;; Store user info in session
  (setf (hunchentoot:session-value *session-user-key*) username)
  (setf (hunchentoot:session-value *session-login-time-key*) (get-universal-time))
  username)

(defun logout-user ()
  "Destroy the current session, logging out the user."
  (when hunchentoot:*session*
        ;; Clear session values
        (hunchentoot:delete-session-value *session-user-key*)
        (hunchentoot:delete-session-value *session-login-time-key*)
        ;; Remove the session entirely
        (hunchentoot:remove-session hunchentoot:*session*))
  t)

(defun require-auth ()
  "Check if user is authenticated. 
   If not, redirects to login page and returns NIL.
   If authenticated, returns T."
  (if (logged-in-p)
      t
      (progn
       ;; Store the originally requested URL for redirect after login
       (when hunchentoot:*session*
             (setf (hunchentoot:session-value :redirect-after-login)
               (hunchentoot:request-uri*)))
       (hunchentoot:redirect "/admin/login")
       nil)))

(defun get-redirect-after-login ()
  "Get the URL to redirect to after successful login, or default to /admin."
  (or (when hunchentoot:*session*
            (prog1
                (hunchentoot:session-value :redirect-after-login)
              (hunchentoot:delete-session-value :redirect-after-login)))
      "/admin"))

;;; Flash messages for login feedback

(defun set-flash-message (message &optional (type :info))
  "Set a flash message to be displayed on the next page load."
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value :flash-message) message)
  (setf (hunchentoot:session-value :flash-type) type))

(defun get-flash-message ()
  "Get and clear the current flash message. Returns (message . type) or NIL."
  (when hunchentoot:*session*
        (let ((message (hunchentoot:session-value :flash-message))
              (type (hunchentoot:session-value :flash-type)))
          (when message
                (hunchentoot:delete-session-value :flash-message)
                (hunchentoot:delete-session-value :flash-type)
                (cons message type)))))
