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

(defgroup embark-sidebar nil
  "Show Embark Collect in a left sidebar."
  :group 'embark)

(defcustom embark-sidebar-width 60
  "Width of the sidebar window."
  :type 'integer)

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
  "Display BUFFER in a left sidebar window. Track window if minor mode is enabled."
  (let ((win
         (display-buffer-in-side-window
          buffer
          `((side . left)
            (slot . 0) (window-width . ,embark-sidebar-width)
            (window-parameters
             .
             ((no-other-window . t)
              (no-delete-other-windows . t)
              (mode-line-format . " *Embark Sidebar*")))))))
    (setq embark-sidebar--window win)
    (message nil)))

;;;###autoload
(defun embark-sidebar-collect-to-sidebar ()
  "Display Embark Collect results in the sidebar buffer. (Suppressed if candidate count too large!)"
  (interactive)
  (let ((source-name "*Embark Source*")
        (sidebar-name embark-sidebar-name)
        buffer
        (candidates
         (and (boundp 'vertico--candidates) vertico--candidates)))
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
                "Too many candidates (%d), not collecting. Threshold is %d."
                (length candidates)
                embark-sidebar-candidate-threshold))))
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
              (setq buffer (get-buffer sidebar-name)))
          (user-error
           (with-current-buffer (get-buffer-create sidebar-name)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert "No candidates found")))
           (setq buffer (get-buffer sidebar-name))))))
    (embark-sidebar--display-buffer buffer)))

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

(defun embark-sidebar--vertico-exit-advice (&rest _)
  "Advice for `vertico-exit' to collect into sidebar if sidebar mode is active."
  (when embark-sidebar-mode
    (embark-sidebar-collect-to-sidebar)))

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
        (embark-sidebar--advice-enable)
        (embark-sidebar-show))
    (setq embark-sidebar--active nil)
    (embark-sidebar--advice-disable)))

(provide 'embark-sidebar)

;;; embark-sidebar.el ends here
