;;; embark-sidebar.el --- Embark Collect Sidebar for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Nobuyuki Kamimoto
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (embark "1.0") (vertico "0.30"))
;; Keywords: convenience, sidebar, embark
;; URL: https://github.com/kn66/embark-sidebar

;;; Commentary:

;; This package provides commands for displaying Embark Collect results in a left-hand sidebar.
;; Includes integration for Vertico completion UI, showing the collect buffer always in a dedicated
;; sidebar window named *Embark Sidebar*.

;;; Code:

(require 'embark)
(require 'vertico)

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
  :type '(repeat symbol))

(defgroup embark-sidebar nil
  "Show Embark Collect in a left sidebar."
  :group 'embark)

(defcustom embark-sidebar-width 60
  "Width of the sidebar window."
  :type 'integer)

(defcustom embark-sidebar-side 'right
  "Which side to display the sidebar on, either `'left` or `'right`."
  :type '(choice (const left) (const right)))

(defcustom embark-sidebar-name "*Embark Sidebar*"
  "Name of the Embark sidebar buffer."
  :type 'string)

(defcustom embark-sidebar-candidate-threshold 600
  "Maximum number of candidates allowed to show in sidebar collect. If the candidate count exceeds this threshold, `embark-collect' is NOT executed."
  :type 'integer)

(defvar embark-sidebar--active nil
  "Non-nil if Embark Sidebar minor mode is enabled.")

(defvar embark-sidebar--window nil
  "Sidebar window object.")

(defvar embark-sidebar--last-command nil
  "Last command executed that triggered the sidebar collect.")

(defun embark-sidebar--clone-collect-buffer (source dest)
  "Clone SOURCE collect buffer fully into DEST buffer (including properties and local variables)."
  (let ((src-buf (get-buffer source))
        (dest-buf (get-buffer-create dest)))
    (with-current-buffer dest-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Completely copy buffer contents and text properties
        (insert-buffer-substring src-buf)

        ;; Set embark-collect-mode
        (embark-collect-mode)

        ;; Inherit important buffer-local variables
        (dolist (sym
                 '(embark--type
                   embark--command
                   embark--target-buffer
                   embark--target-window
                   embark--rerun-function
                   embark--candidates
                   embark--annotate
                   embark--affixate
                   default-directory))
          (when (local-variable-p sym src-buf)
            (set
             (make-local-variable sym)
             (buffer-local-value sym src-buf))))

        ;; Also inherit embark--selection
        (when (local-variable-p 'embark--selection src-buf)
          (set
           (make-local-variable 'embark--selection)
           (buffer-local-value 'embark--selection src-buf)))

        ;; Also inherit keymap
        (when (local-variable-p 'keymap src-buf)
          (set
           (make-local-variable 'keymap)
           (buffer-local-value 'keymap src-buf)))

        ;; Also copy overlays (may contain important metadata)
        (with-current-buffer src-buf
          (dolist (overlay (overlays-in (point-min) (point-max)))
            (let ((start (overlay-start overlay))
                  (end (overlay-end overlay))
                  (props (overlay-properties overlay)))
              (with-current-buffer dest-buf
                (let ((new-overlay (make-overlay start end)))
                  (while props
                    (overlay-put new-overlay (car props) (cadr props))
                    (setq props (cddr props))))))))))))

(defun embark-sidebar--display-buffer (buffer)
  "Display BUFFER in a sidebar window. The side is controlled by `embark-sidebar-side'."
  (let ((win
         (display-buffer-in-side-window
          buffer
          `((side . ,embark-sidebar-side)
            (slot . 0) (window-width . ,embark-sidebar-width)
            (window-parameters
             .
             ((no-other-window . t)
              (no-delete-other-windows . t)
              (mode-line-format . " *Embark Sidebar*")))))))
    (setq embark-sidebar--window win)))

;;;###autoload
(defun embark-sidebar-collect-to-sidebar ()
  "Display Embark Collect results in the sidebar buffer for allowed commands only.
Before collecting, show a message indicating the command used. Suppressed if candidate count too large!"
  (interactive)
  (let* ((source-name "*Embark Source*")
         (sidebar-name embark-sidebar-name)
         buffer
         (candidates
          (and (boundp 'vertico--candidates) vertico--candidates))
         (command
          (or (bound-and-true-p embark--command) last-command)))
    ;; Only run for allowed commands
    (setq embark-sidebar--last-command command)
    (when (member command embark-sidebar-allowed-commands)
      (progn
        ;; Check if candidate count exceeds threshold
        (if (and candidates
                 (numberp embark-sidebar-candidate-threshold)
                 (> (length candidates)
                    embark-sidebar-candidate-threshold))
            (progn
              (with-current-buffer (get-buffer-create sidebar-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert
                   (format
                    "Too many candidates (%d), not collecting. Threshold is %d.\nCommand: %s"
                    (length candidates)
                    embark-sidebar-candidate-threshold
                    command))))
              (setq buffer (get-buffer sidebar-name)))
          ;; Normal embark-collect with cloning
          (progn
            ;; Delete source buffer if exists
            (when (get-buffer source-name)
              (kill-buffer source-name))
            (condition-case err
                (let ((src-buf (embark--collect source-name)))
                  (embark-sidebar--clone-collect-buffer
                   source-name sidebar-name)
                  ;; Keep source buffer (delete after complete cloning)
                  (when (get-buffer source-name)
                    (kill-buffer source-name))
                  (setq buffer (get-buffer sidebar-name))
                  ;; Insert command info at top of sidebar buffer
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t))
                      (goto-char (point-min))
                      (insert (format "Command: %s\n" command)))))
              (user-error
               (with-current-buffer (get-buffer-create sidebar-name)
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert
                    (format "No candidates found.\nCommand: %s"
                            command))))
               (setq buffer (get-buffer sidebar-name))))))
        (embark-sidebar--display-buffer buffer)))))

;;;###autoload
(defun embark-sidebar-close ()
  "Close the Embark Sidebar window and kill the buffer if it exists."
  (interactive)
  (let ((buf (get-buffer embark-sidebar-name)))
    (when-let ((win (and buf (get-buffer-window buf 'visible))))
      (delete-window win))))

;;;###autoload
(defun embark-sidebar-show ()
  "Ensure the sidebar buffer is displayed. If it does not exist, create an empty one."
  (interactive)
  (let ((buffer (get-buffer-create embark-sidebar-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'embark-collect-mode)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (embark-collect-mode)))
    (embark-sidebar--display-buffer buffer)))

;;;###autoload
(defun embark-sidebar-toggle ()
  "Toggle visibility of the Embark sidebar."
  (interactive)
  (let ((buf (get-buffer embark-sidebar-name)))
    (if-let ((win (and buf (get-buffer-window buf 'visible))))
        (delete-window win)
      (when buf
        (embark-sidebar--display-buffer buf)))))

(defun embark-sidebar--auto-switch-window ()
  "Switch to the Embark sidebar window if it exists, otherwise create it."
  (run-at-time
   "0.01 sec" nil
   (lambda ()
     (if (member
          embark-sidebar--last-command
          embark-sidebar-allowed-commands)
         (progn
           (embark-sidebar-show))
       (progn
         (embark-sidebar-close))))))

(defun embark-sidebar--vertico-exit-advice (&rest _)
  "Advice for `vertico-exit' to collect into sidebar if sidebar mode is active."
  (when embark-sidebar-mode
    (embark-sidebar-collect-to-sidebar)
    (embark-sidebar--auto-switch-window)))

(defun embark-sidebar--advice-enable ()
  "Enable advice and hooks for embark-sidebar-mode."
  ;; Vertico advice
  (advice-add
   'vertico-exit
   :before #'embark-sidebar--vertico-exit-advice))

(defun embark-sidebar--advice-disable ()
  "Disable advice and hooks for embark-sidebar-mode."
  (advice-remove 'vertico-exit #'embark-sidebar--vertico-exit-advice)
  (when (get-buffer embark-sidebar-name)
    (when-let ((win
                (get-buffer-window (get-buffer embark-sidebar-name))))
      (delete-window win))))

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
