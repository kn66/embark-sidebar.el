;;; embark-sidebar.el --- Embark Collect Sidebar for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Nobuyuki Kamimoto
;; Version: 1.1
;; Package-Requires: ((emacs "27.1") (embark "1.0") (vertico "0.30"))
;; Keywords: convenience, sidebar, embark
;; URL: https://github.com/kn66/embark-sidebar

;;; Commentary:

;; This package provides commands for displaying Embark Collect results in a left-hand sidebar.
;; Includes integration for Vertico completion UI, showing multiple collect buffers in a dedicated
;; sidebar window with vertical stacking layout using individual side windows.

;;; Code:

(require 'embark)
(require 'vertico)

;;; Customization

(defgroup embark-sidebar nil
  "Show Embark Collect in a left sidebar."
  :group 'embark)

(defcustom embark-sidebar-allowed-commands
  '(consult-line
    consult-imenu
    consult-flymake
    consult-flycheck
    consult-grep
    consult-ripgrep
    consult-xref
    consult-find
    consult-fd
    consult-recent-file
    consult-outline
    consult-global-mark
    consult-project-buffer
    consult-bookmark
    xref-find-definitions
    xref-find-references
    find-file
    project-find-file
    denote-open-or-create)
  "List of commands for which `embark-sidebar-collect-to-sidebar' is enabled."
  :type '(repeat symbol)
  :group 'embark-sidebar)

(defcustom embark-sidebar-width 60
  "Width of the sidebar window."
  :type 'integer
  :group 'embark-sidebar)

(defcustom embark-sidebar-side 'right
  "Which side to display the sidebar on."
  :type '(choice (const left) (const right))
  :group 'embark-sidebar)

(defcustom embark-sidebar-candidate-threshold 600
  "Maximum number of candidates allowed to show in sidebar collect.
If the candidate count exceeds this threshold, `embark-collect' is NOT executed."
  :type 'integer
  :group 'embark-sidebar)

(defcustom embark-sidebar-visible-buffers 3
  "Maximum number of buffers to keep and display in the sidebar.
This controls both the total number of buffers kept in memory and
the number displayed simultaneously. Older buffers are automatically
removed when this limit is exceeded."
  :type 'integer
  :group 'embark-sidebar)

(defcustom embark-sidebar-min-window-height 5
  "Minimum height for each sidebar window."
  :type 'integer
  :group 'embark-sidebar)

;;; Variables

(defvar embark-sidebar--active nil
  "Non-nil if Embark Sidebar minor mode is enabled.")

(defvar embark-sidebar--windows nil
  "List of sidebar window objects.")

(defvar embark-sidebar--buffers nil
  "List of collect buffers being displayed in the sidebar.")

(defvar embark-sidebar--last-command nil
  "Last command executed that triggered the sidebar collect.")

;;; Helper Functions

(defun embark-sidebar--get-current-command ()
  "Get the current command for sidebar operations."
  (or (bound-and-true-p embark--command) last-command))

(defun embark-sidebar--get-candidates ()
  "Get the current candidates from Vertico."
  (and (boundp 'vertico--candidates) vertico--candidates))

(defun embark-sidebar--command-allowed-p (command)
  "Check if COMMAND is allowed for sidebar collection."
  (member command embark-sidebar-allowed-commands))

(defun embark-sidebar--generate-buffer-name (command)
  "Generate a unique buffer name for COMMAND."
  (format "*Embark Collect (%s)*" command))

(defun embark-sidebar--candidates-exceed-threshold-p (candidates)
  "Check if CANDIDATES count exceeds the threshold."
  (and candidates
       (numberp embark-sidebar-candidate-threshold)
       (> (length candidates) embark-sidebar-candidate-threshold)))

(defun embark-sidebar--create-fallback-buffer (message command)
  "Create a fallback buffer with MESSAGE when collect is not available."
  (let ((buffer
         (get-buffer-create
          (embark-sidebar--generate-buffer-name command))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert message))
      ;; Set a simple mode for the fallback buffer
      (when (fboundp 'special-mode)
        (special-mode)))
    buffer))

(defun embark-sidebar--create-threshold-exceeded-message
    (candidates command)
  "Create message for when candidate threshold is exceeded."
  (format
   "Too many candidates (%d), not collecting. Threshold is %d.\nCommand: %s"
   (length candidates) embark-sidebar-candidate-threshold command))

(defun embark-sidebar--create-no-candidates-message (command)
  "Create message for when no candidates are found."
  (format "No candidates found.\nCommand: %s" command))

(defun embark-sidebar--add-command-info-to-buffer (buffer command)
  "Add command information to the top of BUFFER."
  (with-current-buffer buffer
    (when (eq major-mode 'embark-collect-mode)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (insert (format "Command: %s\n" command)))))))

(defun embark-sidebar--close-all-windows ()
  "Close all sidebar windows."
  (dolist (win embark-sidebar--windows)
    (when (window-live-p win)
      (delete-window win)))
  (setq embark-sidebar--windows nil))

(defun embark-sidebar--cleanup-dead-buffers ()
  "Remove dead buffers from the buffer list."
  (setq embark-sidebar--buffers
        (seq-filter #'buffer-live-p embark-sidebar--buffers)))

(defun embark-sidebar--limit-buffers ()
  "Limit the number of buffers to `embark-sidebar-visible-buffers'."
  (embark-sidebar--cleanup-dead-buffers)
  (when (> (length embark-sidebar--buffers)
           embark-sidebar-visible-buffers)
    (let ((buffers-to-remove
           (nthcdr
            embark-sidebar-visible-buffers embark-sidebar--buffers)))
      (dolist (buf buffers-to-remove)
        (when (buffer-live-p buf)
          (kill-buffer buf)))
      (setq embark-sidebar--buffers
            (seq-take embark-sidebar--buffers
                      embark-sidebar-visible-buffers)))))

(defun embark-sidebar--display-multiple-buffers ()
  "Display all collect buffers in vertically stacked sidebar windows."
  (embark-sidebar--close-all-windows)
  (embark-sidebar--cleanup-dead-buffers)

  (when embark-sidebar--buffers
    ;; Create individual side windows for each buffer
    (let ((slot 0))
      (dolist (buffer embark-sidebar--buffers)
        (when (buffer-live-p buffer)
          (let ((win
                 (display-buffer-in-side-window
                  buffer
                  `((side . ,embark-sidebar-side)
                    (slot . ,slot)
                    (window-width . ,embark-sidebar-width)
                    (window-height
                     .
                     ,(max embark-sidebar-min-window-height
                           (/ (frame-height)
                              (length embark-sidebar--buffers))))
                    (window-parameters
                     .
                     ((no-other-window . t)
                      (no-delete-other-windows . t)
                      (mode-line-format
                       . ,(format " %s" (buffer-name buffer)))))))))
            (push win embark-sidebar--windows)
            (setq slot (1+ slot))))))))

(defun embark-sidebar--add-buffer-to-list (buffer)
  "Add BUFFER to the front of the buffer list, removing duplicates."
  (setq embark-sidebar--buffers
        (cons
         buffer
         (seq-remove
          (lambda (b) (eq b buffer)) embark-sidebar--buffers)))
  (embark-sidebar--limit-buffers))

;;; Main Functions

(defun embark-sidebar--handle-threshold-exceeded (candidates command)
  "Handle case where candidate count exceeds threshold."
  (let ((message
         (embark-sidebar--create-threshold-exceeded-message
          candidates command)))
    (embark-sidebar--create-fallback-buffer message command)))

(defun embark-sidebar--handle-normal-collect (command)
  "Handle normal embark collect operation, returning the collect buffer directly."
  (condition-case err
      (progn
        (let* ((buffer-name
                (embark-sidebar--generate-buffer-name command))
               (existing-buffer (get-buffer buffer-name)))
          ;; Kill existing buffer with same name to avoid conflicts
          (when existing-buffer
            (kill-buffer existing-buffer))

          ;; Create collect buffer with command-specific name
          (let ((collect-buffer (embark--collect buffer-name)))
            (when collect-buffer
              ;; Add command info to the collect buffer directly
              (embark-sidebar--add-command-info-to-buffer
               collect-buffer command)
              collect-buffer))))
    (user-error
     (let ((message
            (embark-sidebar--create-no-candidates-message command)))
       (embark-sidebar--create-fallback-buffer message command)))))

;;;###autoload
(defun embark-sidebar-collect-to-sidebar ()
  "Display Embark Collect results in the sidebar using multiple buffers."
  (interactive)
  (let* ((command (embark-sidebar--get-current-command))
         (candidates (embark-sidebar--get-candidates)))

    (setq embark-sidebar--last-command command)

    (when (embark-sidebar--command-allowed-p command)
      (let ((buffer
             (if (embark-sidebar--candidates-exceed-threshold-p
                  candidates)
                 (embark-sidebar--handle-threshold-exceeded
                  candidates command)
               (embark-sidebar--handle-normal-collect command))))
        (when buffer
          ;; Add buffer to the list and display all buffers
          (embark-sidebar--add-buffer-to-list buffer)
          (embark-sidebar--display-multiple-buffers)
          (message "Sidebar updated with buffer: %s"
                   (buffer-name buffer)))))))

;;;###autoload
(defun embark-sidebar-close ()
  "Close the Embark Sidebar windows."
  (interactive)
  (embark-sidebar--close-all-windows))

;;;###autoload
(defun embark-sidebar-show ()
  "Show the sidebar buffers, or create an empty fallback."
  (interactive)
  (if embark-sidebar--buffers
      (embark-sidebar--display-multiple-buffers)
    (let ((buffer
           (embark-sidebar--create-fallback-buffer
            "No collect results available." "none")))
      (embark-sidebar--add-buffer-to-list buffer)
      (embark-sidebar--display-multiple-buffers))))

;;;###autoload
(defun embark-sidebar-toggle ()
  "Toggle visibility of the Embark sidebar."
  (interactive)
  (if (and embark-sidebar--windows
           (seq-some #'window-live-p embark-sidebar--windows))
      (embark-sidebar-close)
    (embark-sidebar-show)))

;;;###autoload
(defun embark-sidebar-refresh ()
  "Refresh the sidebar with the latest collect results."
  (interactive)
  (when embark-sidebar--last-command
    (embark-sidebar-collect-to-sidebar)))

;;;###autoload
(defun embark-sidebar-clear-all ()
  "Clear all collect buffers from the sidebar."
  (interactive)
  (embark-sidebar-close)
  (dolist (buf embark-sidebar--buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq embark-sidebar--buffers nil)
  (message "All sidebar buffers cleared"))

;;;###autoload
(defun embark-sidebar-remove-current ()
  "Remove the currently focused buffer from the sidebar."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (when (member current-buffer embark-sidebar--buffers)
      (setq embark-sidebar--buffers
            (seq-remove
             (lambda (b) (eq b current-buffer))
             embark-sidebar--buffers))
      (kill-buffer current-buffer)
      (if embark-sidebar--buffers
          (embark-sidebar--display-multiple-buffers)
        (embark-sidebar-close))
      (message "Buffer removed from sidebar"))))

;;;###autoload
(defun embark-sidebar-cycle-buffers ()
  "Cycle through sidebar buffers by rotating the buffer order."
  (interactive)
  (when (> (length embark-sidebar--buffers) 1)
    ;; Move the first buffer to the end and refresh display
    (let ((first-buffer (car embark-sidebar--buffers))
          (remaining-buffers (cdr embark-sidebar--buffers)))
      (setq embark-sidebar--buffers
            (append remaining-buffers (list first-buffer)))
      (embark-sidebar--display-multiple-buffers)
      (message "Cycled sidebar buffers (%d total)"
               (length embark-sidebar--buffers)))))

;;;###autoload
(defun embark-sidebar-list-all-buffers ()
  "Show a list of all sidebar buffers and allow selection."
  (interactive)
  (if embark-sidebar--buffers
      (let* ((buffer-names
              (mapcar
               (lambda (buf)
                 (cons (buffer-name buf) buf))
               embark-sidebar--buffers))
             (selected-name
              (completing-read "Select sidebar buffer: " buffer-names
                               nil t))
             (selected-buffer
              (cdr (assoc selected-name buffer-names))))
        (when selected-buffer
          ;; Move selected buffer to front and refresh display
          (setq embark-sidebar--buffers
                (cons
                 selected-buffer
                 (seq-remove
                  (lambda (b) (eq b selected-buffer))
                  embark-sidebar--buffers)))
          (embark-sidebar--display-multiple-buffers)
          (message "Selected buffer moved to front")))
    (message "No sidebar buffers available")))

;;; Auto-switching

(defun embark-sidebar--auto-switch-window ()
  "Switch to the Embark sidebar window if it exists, otherwise create it."
  (run-at-time
   0.01 nil
   (lambda ()
     (if (embark-sidebar--command-allowed-p
          embark-sidebar--last-command)
         (embark-sidebar-show)
       (embark-sidebar-close)))))

;;; Advice Functions

(defun embark-sidebar--vertico-exit-advice (&rest _)
  "Advice for `vertico-exit' to collect into sidebar if sidebar mode is active."
  (when embark-sidebar-mode
    (embark-sidebar-collect-to-sidebar)
    (embark-sidebar--auto-switch-window)))

(defun embark-sidebar--advice-enable ()
  "Enable advice and hooks for embark-sidebar-mode."
  (advice-add
   'vertico-exit
   :before #'embark-sidebar--vertico-exit-advice))

(defun embark-sidebar--advice-disable ()
  "Disable advice and hooks for embark-sidebar-mode."
  (advice-remove 'vertico-exit #'embark-sidebar--vertico-exit-advice)
  (embark-sidebar-close))

;;; Minor Mode

;;;###autoload
(define-minor-mode embark-sidebar-mode
  "Global minor mode to manage Embark sidebar state."
  :init-value nil
  :global t
  :group
  'embark-sidebar
  (if embark-sidebar-mode
      (progn
        (setq embark-sidebar--active t)
        (embark-sidebar--advice-enable))
    (setq embark-sidebar--active nil)
    (embark-sidebar--advice-disable)))

(provide 'embark-sidebar)

;;; embark-sidebar.el ends here
