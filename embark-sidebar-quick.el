;; embark-sidebar-quick.el --- Quick selection for embark-sidebar -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides avy-style quick selection functionality for embark-sidebar.
;; Add this code to your existing embark-sidebar.el file.

;;; Code:

;;; Avy-style selection variables and functions

(require 'embark-sidebar)

(defvar embark-sidebar--selection-keys
  "123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Characters used for avy-style selection.")

(defvar embark-sidebar--overlays nil
  "List of overlays for avy-style selection.")

(defun embark-sidebar--clear-overlays ()
  "Clear all selection overlays."
  (dolist (overlay embark-sidebar--overlays)
    (delete-overlay overlay))
  (setq embark-sidebar--overlays nil))

(defun embark-sidebar--get-candidates-in-buffer (buffer)
  "Get list of candidate positions in BUFFER."
  (with-current-buffer buffer
    (if (derived-mode-p 'embark-collect-mode)
        (save-excursion
          (goto-char (point-min))
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
                ;; More permissive condition - exclude only truly empty lines and command lines
                (unless (or (string-empty-p trimmed-line)
                            (string-match-p "^Command:" trimmed-line)
                            (string-match-p "^[ \t]*$" trimmed-line))
                  (push (list
                         (point) (line-end-position) line-content)
                        candidates))
                (forward-line 1)
                (setq line-count (1+ line-count))))
            (nreverse candidates)))
      nil)))

(defun embark-sidebar--get-candidates-alternative (buffer)
  "Alternative method to get candidates using embark-collect internals."
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
                  (push (list line-start line-end line-content)
                        candidates)))
              (forward-line 1)))
          (nreverse candidates))))))

(defun embark-sidebar--get-candidates-enhanced (buffer)
  "Enhanced candidate detection using multiple methods."
  (let ((method1-candidates
         (embark-sidebar--get-candidates-in-buffer buffer))
        (method2-candidates
         (embark-sidebar--get-candidates-alternative buffer)))
    ;; Use the method that found more candidates, or method2 if both found the same
    (if (> (length method2-candidates) (length method1-candidates))
        method2-candidates
      method1-candidates)))

(defun embark-sidebar--create-selection-overlay (pos key)
  "Create an overlay at POS with selection KEY."
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
  (embark-sidebar--clear-overlays)
  (let ((key-index 0)
        (candidates-with-keys '()))
    ;; Collect all candidates from all buffers using enhanced detection
    (dolist (buffer embark-sidebar--buffers)
      (when (buffer-live-p buffer)
        (let ((buffer-candidates
               (embark-sidebar--get-candidates-enhanced buffer)))
          (with-current-buffer buffer
            (dolist (candidate buffer-candidates)
              (when (< key-index
                       (length embark-sidebar--selection-keys))
                (let* ((pos (car candidate))
                       (key
                        (aref
                         embark-sidebar--selection-keys key-index)))
                  (embark-sidebar--create-selection-overlay pos key)
                  (push (list key buffer pos) candidates-with-keys)
                  (setq key-index (1+ key-index)))))))))
    (nreverse candidates-with-keys)))

(defun embark-sidebar--execute-simple (buffer pos)
  "Simple execution method using keyboard macro."
  (with-current-buffer buffer
    (goto-char pos)
    (condition-case nil
        (progn
          ;; Try pressing RET which should trigger the default action
          (push 'return unread-command-events))
      (error
       nil))))

(defun embark-sidebar--execute-direct (buffer pos)
  "Direct execution by finding and calling the appropriate function."
  (with-current-buffer buffer
    (goto-char pos)
    (let ((line-content (thing-at-point 'line t)))
      (condition-case nil
          (cond
           ;; For file paths, try to open them
           ((and line-content
                 (or (string-match
                      "\\.[a-zA-Z]+[[:space:]]*$" line-content)
                     (string-match "/" line-content)))
            (let ((file-path (string-trim line-content)))
              ;; Extract just the filename/path
              (when (string-match "^\\([^[:space:]]+\\)" file-path)
                (let ((clean-path (match-string 1 file-path)))
                  (find-file clean-path)))))
           ;; For function definitions, try to jump to them
           ((and line-content
                 (string-match
                  "(defun\\|defvar\\|defcustom" line-content))
            (push 'return unread-command-events))
           ;; Default: simulate RET
           (t
            (push 'return unread-command-events)))
        (error
         nil)))))

(defun embark-sidebar--execute-at-position (buffer pos)
  "Execute embark action at POS in BUFFER."
  (with-current-buffer buffer
    (goto-char pos)
    (if (derived-mode-p 'embark-collect-mode)
        (condition-case nil
            ;; Try different methods to execute the action
            (cond
             ;; Method 1: Try embark-act directly
             ((fboundp 'embark-act)
              (call-interactively #'embark-act))
             ;; Method 2: Try direct execution
             (t
              (embark-sidebar--execute-direct buffer pos)))
          (error
           nil))
      nil)))

(defun embark-sidebar--read-selection-key (candidates-with-keys)
  "Read a selection key from user and return corresponding candidate info."
  (let* ((keys (mapcar #'car candidates-with-keys))
         (key-string (mapconcat (lambda (k) (string k)) keys ""))
         (prompt
          (format "Select candidate [%s] (C-g to cancel): "
                  key-string)))
    (condition-case nil
        (let ((input-key (read-char prompt)))
          ;; Check if input is a valid character
          (if (characterp input-key)
              (assoc input-key candidates-with-keys)
            nil))
      ;; Handle quit signal (C-g)
      (quit
       nil)
      ;; Handle other errors
      (error
       nil))))

(defun embark-sidebar--read-selection-key-safe (candidates-with-keys)
  "Read a selection key using read-key for better compatibility."
  (let* ((keys (mapcar #'car candidates-with-keys))
         (key-string (mapconcat (lambda (k) (string k)) keys ""))
         (prompt
          (format "Select candidate [%s] (C-g to cancel): "
                  key-string)))
    (condition-case nil
        (progn
          (message "%s" prompt)
          (let ((input-key (read-key)))
            ;; Handle different types of input
            (cond
             ;; Regular character
             ((characterp input-key)
              (assoc input-key candidates-with-keys))
             ;; Vector form (for special keys)
             ((vectorp input-key)
              (let ((key-char (aref input-key 0)))
                (when (characterp key-char)
                  (assoc key-char candidates-with-keys))))
             ;; Integer form
             ((integerp input-key)
              (assoc input-key candidates-with-keys))
             (t
              nil))))
      (quit
       nil)
      (error
       nil))))

;;;###autoload
(defun embark-sidebar-quick-select ()
  "Quick select candidates using avy-style key display.
Shows numbered/lettered keys on each candidate in the sidebar,
then executes the selected candidate when a key is pressed."
  (interactive)
  (setq-local embark-sidebar--inhibit-close t)
  (embark-sidebar-show)

  (unless embark-sidebar--buffers
    (user-error "No sidebar buffers available"))

  (let ((candidates-with-keys (embark-sidebar--show-selection-keys)))
    (if (null candidates-with-keys)
        (progn
          (embark-sidebar--clear-overlays)
          (message "No candidates found in sidebar"))
      (unwind-protect
          (condition-case err
              (let ((selected
                     (embark-sidebar--read-selection-key
                      candidates-with-keys)))
                (cond
                 ((null selected)
                  (message "Selection cancelled or invalid key"))
                 (t
                  (let ((buffer (nth 1 selected))
                        (pos (nth 2 selected)))
                    (embark-sidebar--execute-at-position buffer pos)
                    (message "Executed candidate")))))
            (quit
             (message "Selection cancelled"))
            (error
             (setq-local embark-sidebar--inhibit-close nil)
             (message "Error during selection: %s"
                      (error-message-string err))))
        ;; Always clear overlays
        (embark-sidebar--clear-overlays))))
  (setq-local embark-sidebar--inhibit-close nil))

;;;###autoload
(defun embark-sidebar-quick-goto ()
  "Quick goto candidates using avy-style key display.
Shows numbered/lettered keys on each candidate in the sidebar,
then moves point to the selected candidate without executing it."
  (interactive)
  (setq-local embark-sidebar--inhibit-close t)
  (unless embark-sidebar--buffers
    (user-error "No sidebar buffers available"))
  (let ((candidates-with-keys (embark-sidebar--show-selection-keys)))
    (if (null candidates-with-keys)
        (progn
          (embark-sidebar--clear-overlays)
          (message "No candidates found in sidebar"))
      (unwind-protect
          (condition-case err
              (let ((selected
                     (embark-sidebar--read-selection-key
                      candidates-with-keys)))
                (cond
                 ((null selected)
                  (message "Selection cancelled or invalid key"))
                 (t
                  (let ((buffer (nth 1 selected))
                        (pos (nth 2 selected)))
                    ;; Switch to the buffer and move point
                    (switch-to-buffer buffer)
                    (goto-char pos)
                    (message "Moved to candidate")))))
            (quit
             (message "Selection cancelled"))
            (error
             (message "Error during selection: %s"
                      (error-message-string err))))
        ;; Always clear overlays
        (embark-sidebar--clear-overlays))))
  (setq-local embark-sidebar--inhibit-close nil))

(provide 'embark-sidebar-quick)

;;; embark-sidebar-quick.el ends here
