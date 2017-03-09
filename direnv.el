;;; direnv.el --- direnv support for emacs

;; Author: Wouter Bolsterlee <wouter@bolsterl.ee>
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.4") (dash "2.13.0") (with-editor "2.5.10"))
;; Keywords: direnv, environment
;; URL: https://github.com/wbolster/emacs-direnv
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides direnv integration for Emacs.
;; See the README for more information.

;;; Code:

(require 'dash)
(require 'json)
(require 'with-editor)

(defvar direnv--output-buffer-name " *direnv*"
  "Name of the hidden buffer used for direnv interaction.")

(defvar direnv--installed (executable-find "direnv")
  "Whether direnv is installed.")

(defvar direnv--active-directory nil
  "Name of the directory for which direnv has most recently ran.")

(defun direnv--export (directory)
  "Call direnv for DIRECTORY and return the parsed result."
  (with-current-buffer (get-buffer-create direnv--output-buffer-name)
    (delete-region (point-min) (point-max))
    (let* ((default-directory directory)
           (exit-code (call-process "direnv" nil '(t nil) nil "export" "json")))
      (unless (zerop exit-code)
        (error "Error running direnv: exit code %s; output was:\n%S"
               exit-code (buffer-string)))
      (unless (zerop (buffer-size))
        (goto-char (point-min))
        (let ((json-key-type 'string))
          (json-read-object))))))

(defun direnv--enable ()
  "Enable direnv mode."
  (add-hook 'post-command-hook #'direnv--maybe-update-environment)
  (direnv--maybe-update-environment))

(defun direnv--disable ()
  "Disable direnv mode."
  (remove-hook 'post-command-hook #'direnv--maybe-update-environment))

(defun direnv--maybe-update-environment ()
  "Maybe update the environment."
  (with-current-buffer (window-buffer)
    (let* ((filename (buffer-file-name (window-buffer))))
      (when (and direnv--installed
                 filename
                 (not (string-equal direnv--active-directory (file-name-directory filename)))
                 (not (file-remote-p filename)))
        (direnv-update-environment filename)))))

(defun direnv--maybe-enable-with-editor-mode ()
  "Enable with-editor-mode when run via direnv-edit."
  ;; This is a dirty hack. See https://github.com/magit/with-editor/issues/23
  (run-at-time
   1 nil
   (lambda ()
     (with-current-buffer (window-buffer)
       (when server-buffer-clients
         (with-editor-mode))))))

;;;###autoload
(defun direnv-update-environment (&optional filename)
  "Update the environment for FILENAME."
  (interactive)
  (let ((filename (or filename buffer-file-name))
        (old-directory direnv--active-directory))
    (unless filename
      (user-error "Buffer is not visiting a file"))
    (when (file-remote-p filename)
      (user-error "Cannot use direnv for remote files"))
    (setq direnv--active-directory (file-name-directory filename))
    (dolist (pair (direnv--export direnv--active-directory))
      (let ((name (car pair))
            (value (cdr pair)))
        (setenv name value)
        (when (string-equal name "PATH")
          (setq exec-path (append (parse-colon-path value) (list exec-directory))))))))

;;;###autoload
(defun direnv-edit ()
  "Edit the .envrc associated with the current directory."
  (interactive)
  (let ((display-buffer-alist
         (cons (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))
               display-buffer-alist)))
    (with-editor-async-shell-command "direnv edit" nil nil))
  (direnv-update-environment))

;;;###autoload
(define-minor-mode direnv-mode
  "Global minor mode to automatically update the environment using direnv.

When this mode is active, the environment inside Emacs will be
continuously updated to match the direnv environment for the currently
visited (local) file."
  :global t
  (if direnv-mode
      (direnv--enable)
    (direnv--disable)))

;;;###autoload
(define-derived-mode direnv-envrc-mode
  sh-mode "envrc"
  "Major mode for .envrc files as used by direnv.

Since .envrc files are shell scripts, this mode inherits from sh-mode.
\\{direnv-envrc-mode-map}")

(add-hook 'direnv-envrc-mode-hook #'direnv--maybe-enable-with-editor-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . direnv-envrc-mode))

(provide 'direnv)

;;; direnv.el ends here
