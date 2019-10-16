;;; direnv.el --- Support for direnv -*- lexical-binding: t; -*-

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Version: 2.0.0
;; Package-Requires: ((emacs "25") (dash "2.12.0"))
;; Keywords: direnv, environment, processes, unix, tools
;; URL: https://github.com/wbolster/emacs-direnv
;;
;; This file is not part of GNU Emacs.

;;; License:

;; 3-clause "new bsd"; see readme for details.

;;; Commentary:

;; This package provides direnv integration for Emacs.
;; See the README for more information.

;;; Code:

(require 'dash)
(require 'json)
(require 'subr-x)

(defgroup direnv nil
  "direnv integration for emacs"
  :group 'environment
  :prefix "direnv-")

(defun direnv--detect ()
  "Detect the direnv executable."
  (executable-find "direnv"))

(defvar direnv--output-buffer-name "*direnv*"
  "Name of the buffer filled with the last direnv output.")

(defvar direnv--installed (direnv--detect)
  "Whether direnv is installed.")

(defvar direnv--active-directory nil
  "Name of the directory for which direnv has most recently ran.")

(defcustom direnv-always-show-summary t
  "Whether to show a summary message of environment changes on every change.

When nil, a summary is only shown when direnv-update-environment is called
interactively."
  :group 'direnv
  :type 'boolean)

(defcustom direnv-show-paths-in-summary t
  "Whether to show directory paths in the summary message."
  :group 'direnv
  :type 'boolean)

(defcustom direnv-use-faces-in-summary t
  "Whether to use custom font faces in the summary message.

When enabled, the summary message uses custom font faces strings
for added, changed, and removed environment variables, which
usually results in coloured output."
  :group 'direnv
  :type 'boolean)

(defcustom direnv-non-file-modes '(eshell-mode dired-mode magit-mode)
  "List of modes where direnv will update even if the buffer has no file.

In these modes, or modes derived from them, direnv will use
  `default-directory'
instead of
  `(file-name-directory (buffer-file-name (current-buffer)))'."
  :group 'direnv
  :type '(repeat (symbol :tag "Major mode")))

(defvar eshell-path-env)

(defun direnv--directory ()
  "Return the relevant directory for the current buffer, or nil."
  (let* ((buffer (or (buffer-base-buffer) (current-buffer)))
         (mode (with-current-buffer buffer major-mode))
         (file-name (buffer-file-name buffer)))
    (cond (file-name
           (file-name-directory file-name))
          ((apply #'provided-mode-derived-p mode direnv-non-file-modes)
           default-directory))))

(defun direnv--export (directory)
  "Call direnv for DIRECTORY and return the parsed result."
  (unless direnv--installed
    (setq direnv--installed (direnv--detect)))
  (unless direnv--installed
    (user-error "Could not find the direnv executable. Is exec-path correct?"))
  (let ((environment process-environment)
        (stderr-tempfile (make-temp-file "direnv-stderr"))) ;; call-process needs a file for stderr output
    (unwind-protect
        (with-current-buffer (get-buffer-create direnv--output-buffer-name)
          (erase-buffer)
          (let* ((default-directory directory)
                 (process-environment environment)
                 (exit-code (call-process "direnv" nil `(t ,stderr-tempfile) nil "export" "json"))
                 (json-key-type 'string))
            (prog1
                (unless (zerop (buffer-size))
                  (goto-char (point-max))
                  (re-search-backward "^{")
                  (json-read-object))
              (unless (zerop (direnv--file-size stderr-tempfile))
                (goto-char (point-max))
                (unless (zerop (buffer-size))
                  (insert "\n\n"))
                (insert-file-contents stderr-tempfile))
              (with-temp-buffer
                (unless (zerop exit-code)
                  (insert-file-contents stderr-tempfile)
                  (warn "Error running direnv (exit code %d):\n%s\nOpen buffer ‘%s’ for full output."
                        exit-code (buffer-string) direnv--output-buffer-name))))))
      (delete-file stderr-tempfile))))

(defun direnv--file-size (name)
  "Get the file size for a file NAME."
  (let ((attributes (file-attributes name)))
    ;; Note: file-attribute-size is Emacs 26+
    (nth 7 attributes)))

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
    (let ((directory-name (direnv--directory)))
      (when (and directory-name
                 (not (file-remote-p directory-name))
                 (not (string-equal direnv--active-directory directory-name))
                 (file-directory-p directory-name))
        (direnv-update-directory-environment directory-name)))))

(defun direnv--summarise-changes (items)
  "Create a summary string for ITEMS."
  (string-join
   (--map
    (let* ((name (car it))
           (state (cdr it))
           (face)
           (prefix))
      (pcase state
        (`added   (setq prefix "+" face 'diff-added))
        (`changed (setq prefix "~" face 'diff-changed))
        (`removed (setq prefix "-" face 'diff-removed)))
      (propertize (concat prefix name) 'face face))
    (--sort
     (string-lessp (symbol-name (cdr it)) (symbol-name (cdr other)))
     (--map
      (cons (car it)
            (if (cdr it) (if (getenv (car it)) 'changed 'added) 'removed))
      (--sort
       (string-lessp (car it) (car other))
       (--remove (string-prefix-p "DIRENV_" (car it)) items)))))
   " "))

(defun direnv--format-paths (old-directory new-directory)
  "Format the path component of the summary message.

The string will describe a transition from OLD-DIRECTORY and
NEW-DIRECTORY, but OLD-DIRECTORY can be nil."
  (cond
   ((or (null old-directory)
        (string-equal old-directory new-directory))
    (abbreviate-file-name (directory-file-name new-directory)))
   (t
    (format "%s → %s"
            (abbreviate-file-name (directory-file-name old-directory))
            (abbreviate-file-name (directory-file-name new-directory))))))

(defun direnv--show-summary (summary old-directory new-directory)
  "Show a SUMMARY message.

OLD-DIRECTORY and NEW-DIRECTORY are the directories before and afther
the environment changes."
  (let ((summary
         (if (string-empty-p summary) "no changes" summary))
        (paths
         (when direnv-show-paths-in-summary
           (direnv--format-paths old-directory new-directory))))
    (unless direnv-use-faces-in-summary
      (setq summary (substring-no-properties summary)))
    (if paths
        (message "direnv: %s (%s)" summary paths)
      (message "direnv: %s" summary))))

;;;###autoload
(defun direnv-update-environment (&optional file-name force-summary)
  "Update the environment for FILE-NAME.

See `direnv-update-directory-environment' for FORCE-SUMMARY."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq force-summary t))
  (direnv-update-directory-environment
   (if file-name (file-name-directory file-name) (direnv--directory))
   force-summary))

;;;###autoload
(defun direnv-update-directory-environment (&optional directory force-summary)
  "Update the environment for DIRECTORY.

When FORCE-SUMMARY is non-nil or when called interactively, show a summary message."
  (interactive)
  (let ((directory (or directory default-directory))
        (old-directory direnv--active-directory)
        (items)
        (summary)
        (show-summary (or force-summary (called-interactively-p 'interactive))))
    (when (file-remote-p directory)
      (user-error "Cannot use direnv for remote files"))
    (setq direnv--active-directory directory
          items (direnv--export direnv--active-directory)
          summary (direnv--summarise-changes items))
    (when (and direnv-always-show-summary (not (string-empty-p summary)))
      (setq show-summary t))
    (when show-summary
      (direnv--show-summary summary old-directory direnv--active-directory))
    (dolist (pair items)
      (let ((name (car pair))
            (value (cdr pair)))
        (setenv name value)
        (when (string-equal name "PATH")
          (setq exec-path (append (parse-colon-path value) (list exec-directory)))
          ;; Prevent `eshell-path-env` getting out-of-sync with $PATH:
          (when (derived-mode-p 'eshell-mode)
            (setq eshell-path-env value)))))))

;;;###autoload
(defun direnv-allow ()
  "Run ‘direnv allow’ and update the environment afterwards."
  (interactive)
  (call-process (direnv--detect) nil 0 nil "allow")
  (direnv-update-environment))

;;;###autoload
(define-minor-mode direnv-mode
  "Global minor mode to automatically update the environment using direnv.

When this mode is active, the environment inside Emacs will be
continuously updated to match the direnv environment for the currently
visited (local) file."
  :require 'direnv
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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . direnv-envrc-mode))

(provide 'direnv)

;;; direnv.el ends here
