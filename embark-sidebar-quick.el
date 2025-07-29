;; embark-sidebar-quick.el --- Quick selection for embark-sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides avy-style quick selection functionality for embark-sidebar.
;; Add this code to your existing embark-sidebar.el file.

;;; Code:

;;; Avy-style selection variables and functions

(require 'embark-sidebar)

(defvar embark-sidebar--selection-keys
  "123456789abcdefghijklmnopqrstuvwxyz"
  "Characters used for avy-style selection.")

(defvar embark-sidebar--overlays nil
  "List of overlays for avy-style selection.")

(defvar embark-sidebar--debug nil
  "Enable debug messages for embark-sidebar selection.")

(defun embark-sidebar--debug-message (format-string &rest args)
  "Print debug message if debugging is enabled."
  (when embark-sidebar--debug
    (message "[DEBUG] %s" (apply #'format format-string args))))

(defun embark-sidebar--clear-overlays ()
  "Clear all selection overlays."
  (embark-sidebar--debug-message "Clearing %d overlays"
                                 (length embark-sidebar--overlays))
  (dolist (overlay embark-sidebar--overlays)
    (delete-overlay overlay))
  (setq embark-sidebar--overlays nil))

(defun embark-sidebar--get-candidates-in-buffer (buffer)
  "Get list of candidate positions in BUFFER."
  (embark-sidebar--debug-message "Getting candidates from buffer: %s"
                                 (buffer-name buffer))
  (with-current-buffer buffer
    (embark-sidebar--debug-message "Buffer mode: %s" major-mode)
    (if (derived-mode-p 'embark-collect-mode)
        (save-excursion
          (goto-char (point-min))
          (embark-sidebar--debug-message
           "Buffer content preview: %s"
           (buffer-substring-no-properties
            (point-min) (min (+ (point-min) 100) (point-max))))
          ;; Skip command info line
          (forward-line 1)
          (let ((candidates '())
                (line-count 0))
            (while (not (eobp))
              (beginning-of-line)
              (let* ((line-content (thing-at-point 'line t))
                     (trimmed-line
                      (if line-content
                          (string-trim line-content)
                        "")))
                (embark-sidebar--debug-message "Line %d: %s"
                                               line-count
                                               trimmed-line)
                ;; More permissive condition - exclude only truly empty lines and command lines
                (unless (or (string-empty-p trimmed-line)
                            (string-match-p "^Command:" trimmed-line)
                            (string-match-p "^[ \t]*$" trimmed-line))
                  (push (list
                         (point) (line-end-position) line-content)
                        candidates)
                  (embark-sidebar--debug-message
                   "Added candidate at pos %d: %s"
                   (point)
                   (substring trimmed-line
                              0 (min 50 (length trimmed-line)))))
                (forward-line 1)
                (setq line-count (1+ line-count))))
            (embark-sidebar--debug-message
             "Found %d candidates in buffer %s"
             (length candidates) (buffer-name buffer))
            (nreverse candidates)))
      (progn
        (embark-sidebar--debug-message
         "Buffer %s is not in embark-collect-mode"
         (buffer-name buffer))
        nil))))

(defun embark-sidebar--get-candidates-alternative (buffer)
  "Alternative method to get candidates using embark-collect internals."
  (embark-sidebar--debug-message
   "Using alternative candidate detection for buffer: %s"
   (buffer-name buffer))
  (with-current-buffer buffer
    (when (derived-mode-p 'embark-collect-mode)
      (save-excursion
        (goto-char (point-min))
        (let ((candidates '()))
          ;; Look for lines that contain actionable content
          (while (not (eobp))
            (beginning-of-line)
            (let* ((line-start (point))
                   (line-end (line-end-position))
                   (line-content
                    (buffer-substring-no-properties
                     line-start line-end)))
              ;; Skip the command line and empty lines
              (unless
                  (or (string-match-p "^Command:" line-content)
                      (string-match-p "^[ \t]*$" line-content)
                      (= line-start (point-min))) ; Skip first line which might be command
                ;; Check if this line has embark-collect properties or is actionable
                (when (> (length (string-trim line-content)) 0)
                  (push
                   (list line-start line-end line-content) candidates)
                  (embark-sidebar--debug-message
                   "Alternative: Added candidate at %d: %s"
                   line-start
                   (substring
                    (string-trim line-content)
                    0 (min 30 (length (string-trim line-content)))))))
              (forward-line 1)))
          (embark-sidebar--debug-message
           "Alternative method found %d candidates"
           (length candidates))
          (nreverse candidates))))))

(defun embark-sidebar--get-candidates-enhanced (buffer)
  "Enhanced candidate detection using multiple methods."
  (let ((method1-candidates
         (embark-sidebar--get-candidates-in-buffer buffer))
        (method2-candidates
         (embark-sidebar--get-candidates-alternative buffer)))

    (embark-sidebar--debug-message
     "Method 1 found %d candidates, Method 2 found %d candidates"
     (length method1-candidates) (length method2-candidates))

    ;; Use the method that found more candidates, or method2 if both found the same
    (if (> (length method2-candidates) (length method1-candidates))
        (progn
          (embark-sidebar--debug-message
           "Using alternative method results")
          method2-candidates)
      (progn
        (embark-sidebar--debug-message
         "Using original method results")
        method1-candidates))))

(defun embark-sidebar--create-selection-overlay (pos key)
  "Create an overlay at POS with selection KEY."
  (embark-sidebar--debug-message
   "Creating overlay at pos %d with key %c"
   pos key)
  (let ((overlay (make-overlay pos (1+ pos))))
    (overlay-put
     overlay 'display
     (propertize (string key)
                 'face
                 '(:foreground
                   "red"
                   :weight bold
                   :background "yellow")))
    (overlay-put overlay 'embark-sidebar-key key)
    (push overlay embark-sidebar--overlays)
    overlay))

(defun embark-sidebar--show-selection-keys ()
  "Show selection keys on all candidates in all sidebar windows."
  (embark-sidebar--debug-message "Starting show-selection-keys")
  (embark-sidebar--debug-message "Available buffers: %s"
                                 (mapcar
                                  #'buffer-name
                                  embark-sidebar--buffers))
  (embark-sidebar--clear-overlays)
  (let ((key-index 0)
        (candidates-with-keys '()))

    ;; Collect all candidates from all buffers using enhanced detection
    (dolist (buffer embark-sidebar--buffers)
      (when (buffer-live-p buffer)
        (embark-sidebar--debug-message "Processing buffer: %s"
                                       (buffer-name buffer))
        (let ((buffer-candidates
               (embark-sidebar--get-candidates-enhanced buffer)))
          (embark-sidebar--debug-message "Buffer %s has %d candidates"
                                         (buffer-name buffer)
                                         (length buffer-candidates))
          (with-current-buffer buffer
            (dolist (candidate buffer-candidates)
              (when (< key-index
                       (length embark-sidebar--selection-keys))
                (let* ((pos (car candidate))
                       (key
                        (aref
                         embark-sidebar--selection-keys key-index)))
                  (embark-sidebar--debug-message
                   "Creating overlay for key %c at pos %d"
                   key pos)
                  (embark-sidebar--create-selection-overlay pos key)
                  (push (list key buffer pos) candidates-with-keys)
                  (setq key-index (1+ key-index)))))))))

    (embark-sidebar--debug-message "Total candidates with keys: %d"
                                   (length candidates-with-keys))
    (nreverse candidates-with-keys)))

;; Additional execution methods
(defun embark-sidebar--execute-simple (buffer pos)
  "Simple execution method using keyboard macro."
  (embark-sidebar--debug-message "Using simple execution method")
  (with-current-buffer buffer
    (goto-char pos)
    (condition-case err
        (progn
          ;; Try pressing RET which should trigger the default action
          (push 'return unread-command-events)
          (embark-sidebar--debug-message "Simulated RET key press"))
      (error
       (embark-sidebar--debug-message "Error in simple execution: %s"
                                      (error-message-string err))))))

(defun embark-sidebar--execute-direct (buffer pos)
  "Direct execution by finding and calling the appropriate function."
  (embark-sidebar--debug-message "Using direct execution method")
  (with-current-buffer buffer
    (goto-char pos)
    (let ((line-content (thing-at-point 'line t)))
      (embark-sidebar--debug-message "Line content: %s" line-content)
      (condition-case err
          (cond
           ;; For file paths, try to open them
           ((and line-content
                 (or (string-match
                      "\\.[a-zA-Z]+[[:space:]]*$" line-content)
                     (string-match "/" line-content)))
            (embark-sidebar--debug-message
             "Detected file path, attempting to open")
            (let ((file-path (string-trim line-content)))
              ;; Extract just the filename/path
              (when (string-match "^\\([^[:space:]]+\\)" file-path)
                (let ((clean-path (match-string 1 file-path)))
                  (embark-sidebar--debug-message "Opening file: %s"
                                                 clean-path)
                  (find-file clean-path)))))
           ;; For function definitions, try to jump to them
           ((and line-content
                 (string-match
                  "(defun\\|defvar\\|defcustom" line-content))
            (embark-sidebar--debug-message "Detected Lisp definition")
            (push 'return unread-command-events))
           ;; Default: simulate RET
           (t
            (embark-sidebar--debug-message
             "Using default RET simulation")
            (push 'return unread-command-events)))
        (error
         (embark-sidebar--debug-message
          "Error in direct execution: %s"
          (error-message-string err)))))))

(defun embark-sidebar--execute-at-position (buffer pos)
  "Execute embark action at POS in BUFFER."
  (embark-sidebar--debug-message "Executing at pos %d in buffer %s"
                                 pos
                                 (buffer-name buffer))
  (with-current-buffer buffer
    (goto-char pos)
    (embark-sidebar--debug-message
     "Current position: %d, current line: %s"
     (point) (thing-at-point 'line t))
    (if (derived-mode-p 'embark-collect-mode)
        (progn
          (embark-sidebar--debug-message "Trying execution methods")
          (condition-case err
              ;; Try different methods to execute the action
              (cond
               ;; Method 1: Try embark-act directly
               ((fboundp 'embark-act)
                (embark-sidebar--debug-message "Using embark-act")
                (call-interactively #'embark-act))
               ;; Method 2: Try direct execution
               (t
                (embark-sidebar--debug-message
                 "Using direct execution")
                (embark-sidebar--execute-direct buffer pos)))
            (error
             (embark-sidebar--debug-message
              "Error in embark action: %s"
              (error-message-string err)))))
      (embark-sidebar--debug-message
       "Buffer is not in embark-collect-mode"))))

(defun embark-sidebar--read-selection-key (candidates-with-keys)
  "Read a selection key from user and return corresponding candidate info."
  (let* ((keys (mapcar #'car candidates-with-keys))
         (key-string (mapconcat (lambda (k) (string k)) keys ""))
         (prompt
          (format "Select candidate [%s] (C-g to cancel): "
                  key-string)))

    (embark-sidebar--debug-message "Available keys: %s" key-string)
    (embark-sidebar--debug-message "Prompt: %s" prompt)

    (condition-case err
        (let ((input-key (read-char prompt)))
          (embark-sidebar--debug-message
           "Input received: %s (type: %s)"
           input-key (type-of input-key))
          ;; Check if input is a valid character
          (if (characterp input-key)
              (let ((result (assoc input-key candidates-with-keys)))
                (embark-sidebar--debug-message "Found candidate: %s"
                                               result)
                result)
            (progn
              (embark-sidebar--debug-message
               "Input is not a character")
              nil)))
      ;; Handle quit signal (C-g)
      (quit
       (embark-sidebar--debug-message "User cancelled with C-g")
       nil)
      ;; Handle other errors
      (error
       (embark-sidebar--debug-message
        "Error in read-selection-key: %s"
        (error-message-string err))
       nil))))

(defun embark-sidebar--read-selection-key-safe (candidates-with-keys)
  "Read a selection key using read-key for better compatibility."
  (let* ((keys (mapcar #'car candidates-with-keys))
         (key-string (mapconcat (lambda (k) (string k)) keys ""))
         (prompt
          (format "Select candidate [%s] (C-g to cancel): "
                  key-string)))

    (embark-sidebar--debug-message "Using safe read method")
    (embark-sidebar--debug-message "Available keys: %s" key-string)

    (condition-case err
        (progn
          (message "%s" prompt)
          (let ((input-key (read-key)))
            (embark-sidebar--debug-message
             "Safe input received: %s (type: %s)"
             input-key (type-of input-key))
            ;; Handle different types of input
            (cond
             ;; Regular character
             ((characterp input-key)
              (embark-sidebar--debug-message "Character input: %c"
                                             input-key)
              (assoc input-key candidates-with-keys))
             ;; Vector form (for special keys)
             ((vectorp input-key)
              (embark-sidebar--debug-message "Vector input: %s"
                                             input-key)
              (let ((key-char (aref input-key 0)))
                (embark-sidebar--debug-message
                 "Extracted character: %s"
                 key-char)
                (when (characterp key-char)
                  (assoc key-char candidates-with-keys))))
             ;; Integer form
             ((integerp input-key)
              (embark-sidebar--debug-message "Integer input: %d"
                                             input-key)
              (assoc input-key candidates-with-keys))
             (t
              (embark-sidebar--debug-message "Unknown input type: %s"
                                             (type-of input-key))
              nil))))
      (quit
       (embark-sidebar--debug-message
        "User cancelled with C-g (safe method)")
       nil)
      (error
       (embark-sidebar--debug-message "Error in safe read method: %s"
                                      (error-message-string err))
       nil))))

;;;###autoload
(defun embark-sidebar-quick-select ()
  "Quick select candidates using avy-style key display.
Shows numbered/lettered keys on each candidate in the sidebar,
then executes the selected candidate when a key is pressed."
  (interactive)
  (setq-local embark-sidebar--inhibit-close t)
  (embark-sidebar-show)

  (embark-sidebar--debug-message
   "=== Starting embark-sidebar-quick-select ===")

  (unless embark-sidebar--buffers
    (embark-sidebar--debug-message "No sidebar buffers available")
    (user-error "No sidebar buffers available"))

  (embark-sidebar--debug-message "Sidebar buffers: %s"
                                 (mapcar
                                  #'buffer-name
                                  embark-sidebar--buffers))

  (let ((candidates-with-keys (embark-sidebar--show-selection-keys)))
    (embark-sidebar--debug-message "Candidates with keys: %d"
                                   (length candidates-with-keys))

    (if (null candidates-with-keys)
        (progn
          (embark-sidebar--debug-message
           "No candidates found, clearing overlays")
          (embark-sidebar--clear-overlays)
          (message "No candidates found in sidebar"))

      (embark-sidebar--debug-message
       "Showing overlays and waiting for input")
      (unwind-protect
          (condition-case err
              (let ((selected
                     (embark-sidebar--read-selection-key
                      candidates-with-keys)))
                (embark-sidebar--debug-message "Selection result: %s"
                                               selected)
                (cond
                 ((null selected)
                  (embark-sidebar--debug-message "No selection made")
                  (message "Selection cancelled or invalid key"))
                 (t
                  (let ((buffer (nth 1 selected))
                        (pos (nth 2 selected)))
                    (embark-sidebar--debug-message
                     "Executing at buffer %s, pos %d"
                     (buffer-name buffer) pos)
                    (embark-sidebar--execute-at-position buffer pos)
                    (message "Executed candidate")))))
            (quit
             (embark-sidebar--debug-message
              "Selection cancelled by user")
             (message "Selection cancelled"))
            (error
             (setq-local embark-sidebar--inhibit-close nil)

             (embark-sidebar--debug-message
              "Error during selection: %s"
              (error-message-string err))
             (message "Error during selection: %s"
                      (error-message-string err))))

        ;; Always clear overlays
        (embark-sidebar--debug-message "Cleaning up overlays")
        (embark-sidebar--clear-overlays))))

  (embark-sidebar--debug-message
   "=== Finished embark-sidebar-quick-select ===")

  (setq-local embark-sidebar--inhibit-close nil))

;;;###autoload
(defun embark-sidebar-quick-goto ()
  "Quick goto candidates using avy-style key display.
Shows numbered/lettered keys on each candidate in the sidebar,
then moves point to the selected candidate without executing it."
  (interactive)
  (setq-local embark-sidebar--inhibit-close t)

  (embark-sidebar--debug-message
   "=== Starting embark-sidebar-quick-goto ===")

  (unless embark-sidebar--buffers
    (embark-sidebar--debug-message "No sidebar buffers available")
    (user-error "No sidebar buffers available"))

  (let ((candidates-with-keys (embark-sidebar--show-selection-keys)))
    (if (null candidates-with-keys)
        (progn
          (embark-sidebar--debug-message "No candidates found")
          (embark-sidebar--clear-overlays)
          (message "No candidates found in sidebar"))

      (unwind-protect
          (condition-case err
              (let ((selected
                     (embark-sidebar--read-selection-key
                      candidates-with-keys)))
                (embark-sidebar--debug-message
                 "Goto selection result: %s"
                 selected)
                (cond
                 ((null selected)
                  (message "Selection cancelled or invalid key"))
                 (t
                  (let ((buffer (nth 1 selected))
                        (pos (nth 2 selected)))
                    (embark-sidebar--debug-message
                     "Moving to buffer %s, pos %d"
                     (buffer-name buffer) pos)
                    ;; Switch to the buffer and move point
                    (switch-to-buffer buffer)
                    (goto-char pos)
                    (message "Moved to candidate")))))
            (quit
             (embark-sidebar--debug-message "Goto cancelled by user")
             (message "Selection cancelled"))
            (error
             (embark-sidebar--debug-message "Error during goto: %s"
                                            (error-message-string
                                             err))
             (message "Error during selection: %s"
                      (error-message-string err))))

        ;; Always clear overlays
        (embark-sidebar--clear-overlays))))

  (setq-local embark-sidebar--inhibit-close nil))

;; Debug control functions
;;;###autoload
(defun embark-sidebar-toggle-debug ()
  "Toggle debug messages for embark-sidebar."
  (interactive)
  (setq embark-sidebar--debug (not embark-sidebar--debug))
  (message "Embark sidebar debug: %s"
           (if embark-sidebar--debug
               "ON"
             "OFF")))

;;;###autoload
(defun embark-sidebar-test-safe-select ()
  "Test version using the safe read method."
  (interactive)
  (embark-sidebar--debug-message "=== Testing safe select method ===")

  (unless embark-sidebar--buffers
    (user-error "No sidebar buffers available"))

  (let ((candidates-with-keys (embark-sidebar--show-selection-keys)))
    (if (null candidates-with-keys)
        (progn
          (embark-sidebar--clear-overlays)
          (message "No candidates found in sidebar"))

      (unwind-protect
          (let ((selected
                 (embark-sidebar--read-selection-key-safe
                  candidates-with-keys)))
            (embark-sidebar--debug-message "Safe selection result: %s"
                                           selected)
            (if selected
                (let ((buffer (nth 1 selected))
                      (pos (nth 2 selected)))
                  (embark-sidebar--execute-at-position buffer pos)
                  (message "Executed candidate (safe method)"))
              (message "No candidate selected (safe method)")))

        (embark-sidebar--clear-overlays)))))

(provide 'embark-sidebar-quick)

;;; embark-sidebar-quick.el ends here
