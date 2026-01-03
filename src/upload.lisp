;;;; upload.lisp - File upload handling for Music Manager
;;;; MP3 file validation and storage

(in-package :music-manager)

;;; MP3 file magic bytes
;;; ID3v2 tag starts with "ID3"
;;; MP3 frame sync starts with 0xFF followed by 0xFB, 0xFA, or 0xF3
(defparameter *id3-magic* #(73 68 51) ; "ID3" in ASCII
              "Magic bytes for ID3v2 tag header")

(defparameter *mp3-frame-sync-byte* #xFF
              "First byte of MP3 frame sync")

(defparameter *mp3-frame-second-bytes* '(#xFB #xFA #xF3 #xF2 #xE3 #xE2)
              "Valid second bytes for MP3 frame sync (different MPEG versions/layers)")

(defun get-upload-directory ()
  "Get the path to the mp3 upload directory."
  (let ((dir (merge-pathnames "mp3/" (asdf:system-source-directory :music-manager))))
    ;; Ensure directory exists
    (ensure-directories-exist dir)
    dir))

(defun valid-mp3-extension-p (filename)
  "Check if the filename has a .mp3 extension (case-insensitive)."
  (let ((name (string-downcase (or filename ""))))
    (and (> (length name) 4)
         (string= ".mp3" (subseq name (- (length name) 4))))))

(defun read-file-header (path &optional (num-bytes 10))
  "Read the first NUM-BYTES from a file. Returns a vector of bytes."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array num-bytes :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun bytes-match-p (buffer pattern)
  "Check if the start of BUFFER matches PATTERN."
  (and (>= (length buffer) (length pattern))
       (loop for i from 0 below (length pattern)
               always (= (aref buffer i) (aref pattern i)))))

(defun valid-mp3-magic-p (path)
  "Check if the file has valid MP3 magic bytes (ID3 tag or frame sync)."
  (handler-case
      (let ((header (read-file-header path 3)))
        (or
         ;; Check for ID3v2 tag
         (bytes-match-p header *id3-magic*)
         ;; Check for MP3 frame sync
         (and (= (aref header 0) *mp3-frame-sync-byte*)
              (member (aref header 1) *mp3-frame-second-bytes*))))
    (error () nil)))

(defun validate-mp3 (path original-name)
  "Validate an MP3 file. Returns (values valid-p error-message)."
  (cond
   ;; Check extension
   ((not (valid-mp3-extension-p original-name))
     (values nil "File must have .mp3 extension"))
   ;; Check file exists
   ((not (probe-file path))
     (values nil "File not found"))
   ;; Check magic bytes
   ((not (valid-mp3-magic-p path))
     (values nil "File is not a valid MP3 (invalid header)"))
   ;; All checks passed
   (t (values t nil))))

(defun generate-unique-filename (original-name)
  "Generate a unique filename to prevent conflicts.
   Format: timestamp-random-originalname.mp3"
  (let* ((base (pathname-name original-name))
         (timestamp (get-universal-time))
         (random-part (random 100000)))
    (format nil "~A-~A-~A.mp3" timestamp random-part base)))

(defun save-uploaded-file (temp-path original-name)
  "Save an uploaded file to the mp3 directory after validation.
   Returns (values success-p filename-or-error)."
  (multiple-value-bind (valid-p error-msg)
      (validate-mp3 temp-path original-name)
    (if (not valid-p)
        (values nil error-msg)
        (let* ((new-filename (generate-unique-filename original-name))
               (dest-path (merge-pathnames new-filename (get-upload-directory))))
          (handler-case
              (progn
               ;; Copy file to destination
               (uiop:copy-file temp-path dest-path)
               (values t new-filename))
            (error (e)
              (values nil (format nil "Failed to save file: ~A" e))))))))

(defun delete-uploaded-file (filename)
  "Delete an uploaded MP3 file."
  (let ((path (merge-pathnames filename (get-upload-directory))))
    (when (probe-file path)
          (delete-file path)
          t)))

(defun get-mp3-url (filename)
  "Get the public URL for an MP3 file."
  (format nil "/mp3/~A" filename))

(defun process-multipart-upload (post-parameter)
  "Process a multipart file upload from Hunchentoot.
   POST-PARAMETER should be the result of (hunchentoot:post-parameter \"file\").
   Returns (values success-p result) where result is filename or error message."
  (if (and post-parameter (listp post-parameter))
      (let ((temp-path (first post-parameter))
            (original-name (second post-parameter)))
        (save-uploaded-file temp-path original-name))
      (values nil "No file uploaded")))
