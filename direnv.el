;;; direnv.el --- direnv support for emacs

;; Author: Wouter Bolsterlee <wouter@bolsterl.ee>
;; Version: 1.2.1
;; Package-Requires: ((emacs "24.4") (dash "2.12.0") (with-editor "2.5.10"))
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

(defvar direnv--output-buffer-name " *direnv*"
  "Name of the hidden buffer used for direnv interaction.")

(defvar direnv--installed (direnv--detect)
  "Whether direnv is installed.")

(defvar direnv--active-directory nil
  "Name of the directory for which direnv has most recently ran.")

(defcustom direnv-always-show-summary nil
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

(defcustom direnv-non-file-modes nil
  "List of modes where direnv will update even if the buffer has no file.

In these modes, direnv will use `default-directory' instead of
`(file-name-directory (buffer-file-name (current-buffer)))'."
  :group 'direnv
  :type '(repeat function))

(defun direnv--directory ()
  "Return the relevant directory for the current buffer, or nil."
  (let ((f (buffer-file-name (current-buffer))))
    (cond (f (file-name-directory f))
          ((member major-mode direnv-non-file-modes) default-directory))))

(defun direnv--export (directory)
  "Call direnv for DIRECTORY and return the parsed result."
  (unless direnv--installed
    (setq direnv--installed (direnv--detect)))
  (unless direnv--installed
    (user-error "Could not find the direnv executable. Is exec-path correct?"))
  (with-current-buffer (get-buffer-create direnv--output-buffer-name)
    (erase-buffer)
    (let* ((default-directory directory)
           (exit-code (call-process "direnv" nil '(t t) nil "export" "json")))
      (unless (zerop exit-code)
        (display-buffer (current-buffer))
        (error "Error running direnv: exit code %s; output is in buffer '%s'"
               exit-code direnv--output-buffer-name))
      (unless (zerop (buffer-size))
        (goto-char (point-max))
        (re-search-backward "^{")
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
    (let ((directory-name (direnv--directory)))
      (when (and directory-name
                 (file-directory-p directory-name)
                 (not (string-equal direnv--active-directory directory-name))
                 (not (file-remote-p directory-name)))
        (direnv-update-directory-environment directory-name)))))

(defun direnv--maybe-enable-with-editor-mode ()
  "Enable with-editor-mode when run via direnv-edit."
  ;; This is a dirty hack. See https://github.com/magit/with-editor/issues/23
  (run-at-time
   1 nil
   (lambda ()
     (with-current-buffer (window-buffer)
       (when server-buffer-clients
         (with-editor-mode))))))

(defun direnv--summarise-changes (items)
  "Create a summary string for ITEMS."
  (string-join
   (--map
    (let* ((name (car it))
           (state (cdr it))
           (face)
           (prefix))
      (pcase state
        ('added   (setq prefix "+" face 'diff-added))
        ('changed (setq prefix "~" face 'diff-changed))
        ('removed (setq prefix "-" face 'diff-removed)))
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

(defun direnv--show-summary (items old-directory new-directory)
  "Show a summary message for ITEMS.

OLD-DIRECTORY and NEW-DIRECTORY are the directories before and afther
the environment changes."
  (let ((summary (direnv--summarise-changes items))
        (paths (format
                " (%s)"
                (if (and old-directory (string-equal old-directory new-directory))
                    new-directory
                  (format "from %s to %s" (or old-directory "(none)") new-directory)))))
    (when (string-empty-p summary)
      (setq summary "no changes"))
    (unless direnv-show-paths-in-summary
      (setq paths ""))
    (unless direnv-use-faces-in-summary
      (setq summary (substring-no-properties summary)))
    (message "direnv: %s%s" summary paths)))

;;;###autoload
(defun direnv-update-environment (&optional file-name)
  "Update the environment for FILE-NAME."
  (interactive)
  (direnv-update-directory-environment
   (if file-name (file-name-directory file-name) (direnv--directory))
   (called-interactively-p 'interactive)))

;;;###autoload
(defun direnv-update-directory-environment (&optional directory force-summary)
  "Update the environment for DIRECTORY."
  (interactive)
  (let ((directory (or directory default-directory))
        (old-directory direnv--active-directory))
    (when (file-remote-p directory)
      (user-error "Cannot use direnv for remote files"))
    (setq direnv--active-directory directory)
    (let ((items (direnv--export direnv--active-directory)))
      (when (or direnv-always-show-summary force-summary
                (called-interactively-p 'interactive))
        (direnv--show-summary items old-directory direnv--active-directory))
      (dolist (pair items)
        (let ((name (car pair))
              (value (cdr pair)))
          (setenv name value)
          (when (string-equal name "PATH")
            (setq exec-path (append (parse-colon-path value) (list exec-directory)))))))))

;;;###autoload
(defun direnv-edit ()
  "Edit the .envrc associated with the current directory."
  (interactive)
  (require 'with-editor)
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

(add-hook 'direnv-envrc-mode-hook #'direnv--maybe-enable-with-editor-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . direnv-envrc-mode))

(provide 'direnv)

;;; direnv.el ends here
