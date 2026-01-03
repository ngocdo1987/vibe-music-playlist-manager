;;;; main-test.lisp - Tests for Music Manager
;;;; Unit tests for core functionality

(defpackage :music-manager/tests
  (:use :cl :fiveam)
  (:export :music-manager-tests))

(in-package :music-manager/tests)

;;; Define test suite
(def-suite music-manager-tests
           :description "Music Manager test suite")

(in-suite music-manager-tests)

;;; ============================================================
;;; MP3 Validation Tests
;;; ============================================================

(test mp3-extension-valid
      "Test that .mp3 extension is correctly validated"
      (is (music-manager::valid-mp3-extension-p "song.mp3"))
      (is (music-manager::valid-mp3-extension-p "My Song.MP3"))
      (is (music-manager::valid-mp3-extension-p "test.Mp3")))

(test mp3-extension-invalid
      "Test that non-.mp3 extensions are rejected"
      (is-false (music-manager::valid-mp3-extension-p "song.wav"))
      (is-false (music-manager::valid-mp3-extension-p "song.mp4"))
      (is-false (music-manager::valid-mp3-extension-p "song"))
      (is-false (music-manager::valid-mp3-extension-p ""))
      (is-false (music-manager::valid-mp3-extension-p nil)))

(test mp3-magic-bytes-id3
      "Test that ID3 header is recognized"
      ;; Create a temp file with ID3 header
      (let ((temp-path (merge-pathnames "test-id3.mp3" (uiop:temporary-directory))))
        (with-open-file (stream temp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          ;; Write ID3 header
          (write-byte 73 stream) ; 'I'
          (write-byte 68 stream) ; 'D'
          (write-byte 51 stream)) ; '3'
        (unwind-protect
            (is (music-manager::valid-mp3-magic-p temp-path))
          (when (probe-file temp-path)
                (delete-file temp-path)))))

(test mp3-magic-bytes-frame-sync
      "Test that MP3 frame sync is recognized"
      (let ((temp-path (merge-pathnames "test-sync.mp3" (uiop:temporary-directory))))
        (with-open-file (stream temp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          ;; Write MP3 frame sync
          (write-byte #xFF stream)
          (write-byte #xFB stream)
          (write-byte #x90 stream))
        (unwind-protect
            (is (music-manager::valid-mp3-magic-p temp-path))
          (when (probe-file temp-path)
                (delete-file temp-path)))))

(test mp3-magic-bytes-invalid
      "Test that invalid headers are rejected"
      (let ((temp-path (merge-pathnames "test-invalid.mp3" (uiop:temporary-directory))))
        (with-open-file (stream temp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          ;; Write invalid header
          (write-byte 0 stream)
          (write-byte 0 stream)
          (write-byte 0 stream))
        (unwind-protect
            (is-false (music-manager::valid-mp3-magic-p temp-path))
          (when (probe-file temp-path)
                (delete-file temp-path)))))

;;; ============================================================
;;; Configuration Tests
;;; ============================================================

(test config-parse-env-line
      "Test .env line parsing"
      (is (equal (music-manager::parse-env-line "KEY=value")
                 '("KEY" . "value")))
      (is (equal (music-manager::parse-env-line "KEY = value ")
                 '("KEY" . "value")))
      (is (equal (music-manager::parse-env-line "KEY=\"quoted value\"")
                 '("KEY" . "quoted value")))
      (is-false (music-manager::parse-env-line "# comment"))
      (is-false (music-manager::parse-env-line ""))
      (is-false (music-manager::parse-env-line "   ")))

;;; ============================================================
;;; Utility Tests
;;; ============================================================

(test unique-filename-generation
      "Test that generated filenames are unique"
      (let ((name1 (music-manager::generate-unique-filename "test.mp3"))
            (name2 (music-manager::generate-unique-filename "test.mp3")))
        (is (stringp name1))
        (is (stringp name2))
        (is (search ".mp3" name1))
        ;; Filenames should be different due to random component
        (is (not (string= name1 name2)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(defun run-tests ()
  "Run all tests and return results"
  (run! 'music-manager-tests))
